//> using scala 3.9.0
//> using toolkit 0.9.2

import sttp.client4.*

import java.time.OffsetDateTime
import java.time.ZoneId
import java.time.format.DateTimeFormatter

private val ApiBase = uri"https://wapi.boappa.se"
private val Stockholm = ZoneId.of("Europe/Stockholm")
private val TimeFmt = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")
private val PageSize = 20

case class Config(
    token: Option[String] = None,
    tokenFile: Option[os.Path] = None,
    out: os.Path = os.pwd / "boappa-chat-archive.md",
    types: String = "board"
)

case class Room(
    id: String,
    name: String,
    adminId: Option[String],
    isPrivate: Boolean,
    roomType: Option[String],
    createdAt: Option[OffsetDateTime],
    lastMessageDate: Option[Long]
)

case class ChatMessage(
    id: String,
    authorId: Option[String],
    text: String,
    createdAt: Option[OffsetDateTime],
    updatedAt: Option[OffsetDateTime],
    mediaIds: Vector[String]
)

extension (v: ujson.Value)
  private def field(key: String): Option[ujson.Value] =
    v.objOpt.flatMap(_.get(key)).filterNot(_ == ujson.Null)
  private def strOpt(key: String): Option[String] = field(key).flatMap(_.strOpt)
  private def timeOpt(key: String): Option[OffsetDateTime] =
    strOpt(key).flatMap(s => scala.util.Try(OffsetDateTime.parse(s)).toOption)

private def parseRoom(v: ujson.Value): Room = Room(
  id = v.strOpt("_id").getOrElse(sys.error("room without _id")),
  name = v.strOpt("name").map(_.trim).filter(_.nonEmpty).getOrElse("(unnamed)"),
  adminId = v.strOpt("adminId"),
  isPrivate = v.field("isPrivate").flatMap(_.boolOpt).getOrElse(false),
  roomType = v.strOpt("type"),
  createdAt = v.timeOpt("createdAt"),
  lastMessageDate = v.field("lastMessageDate").flatMap(_.numOpt).map(_.toLong)
)

private def parseMessage(v: ujson.Value): ChatMessage = ChatMessage(
  id = v.strOpt("_id").getOrElse(v.render()),
  authorId = v.strOpt("authorId"),
  text = v.strOpt("message").getOrElse(""),
  createdAt = v.timeOpt("createdAt"),
  updatedAt = v.timeOpt("updatedAt"),
  mediaIds = v.field("mediaIds").flatMap(_.arrOpt).map(_.flatMap(_.strOpt).toVector).getOrElse(Vector.empty)
)

/** The only author details the API exposes are on each room's `lastMessage.author`. */
private def authorNames(rawRooms: Seq[ujson.Value]): Map[String, String] =
  rawRooms.flatMap { r =>
    for
      last <- r.field("lastMessage")
      author <- last.field("author")
      id <- author.strOpt("_id")
      name = s"${author.strOpt("name").getOrElse("")} ${author.strOpt("surname").getOrElse("")}".trim
      if name.nonEmpty
    yield id -> name
  }.toMap

class BoappaChatClient(token: String, backend: SyncBackend):
  private def getData(uri: sttp.model.Uri): Vector[ujson.Value] =
    val response = basicRequest
      .get(uri)
      .header("Authorization", s"Bearer $token")
      .header("Accept", "application/json, text/plain, */*")
      .header("Origin", "https://app.boappa.se")
      .response(asStringOrFail)
      .send(backend)
    val json = ujson.read(response.body)
    if !json.field("ok").flatMap(_.boolOpt).getOrElse(false) then
      sys.error(s"API error for $uri: ${json.field("err").map(_.render()).getOrElse("unknown")}")
    json.field("data").flatMap(_.arrOpt).map(_.toVector).getOrElse(Vector.empty)

  /** Cursor pagination via `lastKnownDate`; stops when a page brings nothing new. */
  private def paginate(fetch: String => Vector[ujson.Value], idOf: ujson.Value => String)(
      nextCursor: Vector[ujson.Value] => Option[String]
  ): Vector[ujson.Value] =
    val collected = scala.collection.mutable.LinkedHashMap.empty[String, ujson.Value]
    var cursor: Option[String] = Some("undefined")
    while cursor.isDefined do
      val items = fetch(cursor.get)
      val fresh = items.filterNot(v => collected.contains(idOf(v)))
      fresh.foreach(v => collected(idOf(v)) = v)
      cursor = if fresh.isEmpty then None else nextCursor(items)
    collected.values.toVector

  def rawRooms(types: String): Vector[ujson.Value] =
    paginate(
      c => getData(ApiBase.addPath("chat", "get-board-rooms").addParams("types" -> types, "lastKnownDate" -> c)),
      _.strOpt("_id").getOrElse("")
    )(items => items.flatMap(_.field("lastMessageDate").flatMap(_.numOpt)).minOption.map(_.toLong.toString))

  def messages(roomId: String): Vector[ChatMessage] =
    paginate(
      c =>
        getData(
          ApiBase
            .addPath("chat", "get-room-messages", roomId)
            .addParams("lastKnownDate" -> c, "limit" -> PageSize.toString)
        ),
      _.strOpt("_id").getOrElse("")
    // UTC ISO timestamps with a fixed format sort lexicographically
    )(items => items.flatMap(_.strOpt("createdAt")).minOption).map(parseMessage)

private def fmt(t: Option[OffsetDateTime]): String =
  t.map(_.atZoneSameInstant(Stockholm).format(TimeFmt)).getOrElse("unknown date")

private def quoted(text: String): String =
  if text.isBlank then "_(no text)_"
  else text.linesIterator.map(l => if l.isBlank then ">" else s"> $l").mkString("\n")

private def renderRoom(room: Room, messages: Vector[ChatMessage], names: Map[String, String]): String =
  def who(id: Option[String]): String = id.map(i => names.getOrElse(i, s"User `$i`")).getOrElse("Unknown")
  val sb = StringBuilder()
  sb ++= s"## ${room.name}\n\n"
  room.roomType.foreach(t => sb ++= s"- **Type:** $t${if room.isPrivate then " (private)" else ""}\n")
  room.adminId.foreach(a => sb ++= s"- **Admin:** ${who(Some(a))}\n")
  sb ++= s"- **Created:** ${fmt(room.createdAt)}\n"
  sb ++= s"- **Messages:** ${messages.size}\n"
  sb ++= s"- **Room id:** `${room.id}`\n\n"
  if messages.isEmpty then sb ++= "_(no messages)_\n\n"
  else
    for m <- messages do
      sb ++= s"### ${who(m.authorId)} — ${fmt(m.createdAt)}\n\n"
      sb ++= quoted(m.text)
      sb ++= "\n"
      if m.updatedAt.exists(u => m.createdAt.forall(c => u.toInstant.isAfter(c.toInstant.plusSeconds(1)))) then
        sb ++= s"\n_Edited ${fmt(m.updatedAt)}_\n"
      if m.mediaIds.nonEmpty then
        sb ++= "\n**Media:**\n\n"
        m.mediaIds.foreach(id => sb ++= s"- `$id`\n")
      sb ++= "\n"
  sb.result()

private def parseArgs(args: Seq[String]): Config =
  def loop(rest: List[String], acc: Config): Config = rest match
    case "--token" :: v :: tail      => loop(tail, acc.copy(token = Some(v)))
    case "--token-file" :: v :: tail => loop(tail, acc.copy(tokenFile = Some(os.Path(v, os.pwd))))
    case "--out" :: v :: tail        => loop(tail, acc.copy(out = os.Path(v, os.pwd)))
    case "--types" :: v :: tail      => loop(tail, acc.copy(types = v))
    case unknown :: _                => sys.error(s"Unknown argument: $unknown")
    case Nil                         => acc
  loop(args.toList, Config())

private def resolveToken(cfg: Config): String =
  cfg.token
    .orElse(cfg.tokenFile.map(p => os.read(p).trim))
    .orElse(sys.env.get("BOAPPA_TOKEN"))
    .map(_.stripPrefix("Bearer ").trim)
    .filter(_.nonEmpty)
    .getOrElse(sys.error("No token: pass --token/--token-file or set BOAPPA_TOKEN"))

@main def boappaChatArchive(args: String*): Unit =
  val cfg = parseArgs(args)
  val backend = DefaultSyncBackend()
  try
    val client = BoappaChatClient(resolveToken(cfg), backend)
    val raw = client.rawRooms(cfg.types)
    val names = authorNames(raw)
    val rooms = raw.map(parseRoom).sortBy(_.createdAt.map(_.toInstant).getOrElse(java.time.Instant.MIN))
    System.err.println(s"Fetched ${rooms.size} rooms")

    val sb = StringBuilder()
    sb ++= "# Boappa chat archive\n\n"
    sb ++= s"- **Exported:** ${fmt(Some(OffsetDateTime.now()))}\n"
    sb ++= s"- **Rooms:** ${rooms.size} (types: ${cfg.types})\n\n"
    sb ++= "Media are referenced by Boappa media id only; the API does not expose their URLs here.\n\n"
    sb ++= "---\n\n"

    for (room, i) <- rooms.zipWithIndex do
      System.err.println(s"[${i + 1}/${rooms.size}] ${room.name}")
      val messages = client.messages(room.id).sortBy(_.createdAt.map(_.toInstant).getOrElse(java.time.Instant.MIN))
      sb ++= renderRoom(room, messages, names)
      sb ++= "---\n\n"

    os.write.over(cfg.out, sb.result())
    println(s"Wrote ${cfg.out}")
  finally backend.close()
