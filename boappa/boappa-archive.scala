//> using scala 3.9.0
//> using toolkit 0.9.2

import sttp.client4.*

import java.time.OffsetDateTime
import java.time.ZoneId
import java.time.format.DateTimeFormatter

private val ApiBase = uri"https://wapi.boappa.se"
private val DefaultSociety = "61a4d8c153efdd0f581f1bad"
private val Stockholm = ZoneId.of("Europe/Stockholm")
private val TimeFmt = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")

case class Config(
    society: String = DefaultSociety,
    token: Option[String] = None,
    tokenFile: Option[os.Path] = None,
    out: os.Path = os.pwd / "boappa-archive.md",
    statuses: String = "open,closed"
)

case class Person(id: String, name: String, surname: String):
  def display: String = s"$name $surname".trim match
    case "" => id
    case s  => s

case class Message(
    id: String,
    author: Option[Person],
    text: String,
    createdAt: Option[OffsetDateTime],
    attachments: Int
)

case class Ticket(
    id: String,
    number: Int,
    title: String,
    description: String,
    status: String,
    variant: Option[String],
    space: Option[String],
    customer: Option[Person],
    assignedUser: Option[Person],
    createdAt: Option[OffsetDateTime],
    updatedAt: Option[OffsetDateTime]
)

extension (v: ujson.Value)
  private def field(key: String): Option[ujson.Value] =
    v.objOpt.flatMap(_.get(key)).filterNot(_ == ujson.Null)
  private def strOpt(key: String): Option[String] = field(key).flatMap(_.strOpt)
  private def timeOpt(key: String): Option[OffsetDateTime] =
    strOpt(key).flatMap(s => scala.util.Try(OffsetDateTime.parse(s)).toOption)

private def parsePerson(v: Option[ujson.Value]): Option[Person] =
  v.map(p => Person(p.strOpt("id").getOrElse(""), p.strOpt("name").getOrElse(""), p.strOpt("surname").getOrElse("")))

private def parseTicket(v: ujson.Value): Ticket = Ticket(
  id = v.strOpt("id").getOrElse(sys.error("ticket without id")),
  number = v.field("ticketNumber").flatMap(_.numOpt).map(_.toInt).getOrElse(0),
  title = v.strOpt("title").getOrElse("(untitled)"),
  description = v.strOpt("description").getOrElse(""),
  status = v.strOpt("status").getOrElse("unknown"),
  variant = v.strOpt("variant"),
  space = v.strOpt("space"),
  customer = parsePerson(v.field("customer")),
  assignedUser = parsePerson(v.field("assignedUser")),
  createdAt = v.timeOpt("createdAt"),
  updatedAt = v.timeOpt("updatedAt")
)

private def parseMessage(v: ujson.Value): Message = Message(
  id = v.strOpt("id").getOrElse(""),
  author = parsePerson(v.field("author")),
  text = v.strOpt("message").getOrElse(""),
  createdAt = v.timeOpt("createdAt"),
  attachments =
    v.field("media").flatMap(_.arrOpt).map(_.size).getOrElse(0) +
      v.field("documents").flatMap(_.arrOpt).map(_.size).getOrElse(0)
)

class BoappaClient(token: String, backend: SyncBackend):
  /** Fetches every page of a paginated endpoint. Page indexing is ambiguous (page=0 and page=1 overlap),
    * so pages are requested past the reported end and items are de-duplicated by id.
    */
  private def pagedItems(path: Seq[String], extraParams: Map[String, String]): Vector[ujson.Value] =
    def page(n: Int): (Vector[ujson.Value], Int) =
      val uri = ApiBase.addPath(path).addParams(extraParams + ("page" -> n.toString))
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
      val data = json("data")
      val items = data.field("items").flatMap(_.arrOpt).map(_.toVector).getOrElse(Vector.empty)
      (items, data.field("pages").flatMap(_.numOpt).map(_.toInt).getOrElse(1))

    val (first, totalPages) = page(0)
    val collected = scala.collection.mutable.LinkedHashMap.from(first.map(v => v.strOpt("id").getOrElse(v.render()) -> v))
    var n = 1
    var exhausted = false
    while !exhausted && n <= totalPages do
      val (items, _) = page(n)
      if items.isEmpty then exhausted = true
      else items.foreach(v => collected.getOrElseUpdate(v.strOpt("id").getOrElse(v.render()), v))
      n += 1
    collected.values.toVector

  def tickets(society: String, statuses: String): Vector[Ticket] =
    pagedItems(Seq(society, "ticket", "board"), Map("status" -> statuses)).map(parseTicket)

  def messages(society: String, ticketId: String): Vector[Message] =
    pagedItems(Seq(society, "ticket", "board", ticketId, "messages"), Map.empty).map(parseMessage)

private def fmt(t: Option[OffsetDateTime]): String =
  t.map(_.atZoneSameInstant(Stockholm).format(TimeFmt)).getOrElse("unknown date")

private def quoted(text: String): String =
  if text.isBlank then "_(no text)_"
  else text.linesIterator.map(l => if l.isBlank then ">" else s"> $l").mkString("\n")

private def renderTicket(t: Ticket, messages: Vector[Message]): String =
  val sb = StringBuilder()
  sb ++= s"## #${t.number} — ${t.title}\n\n"
  sb ++= s"- **Status:** ${t.status}\n"
  t.variant.foreach(v => sb ++= s"- **Type:** $v\n")
  t.space.foreach(s => sb ++= s"- **Space:** $s\n")
  t.customer.foreach(c => sb ++= s"- **Reporter:** ${c.display}\n")
  t.assignedUser.foreach(a => sb ++= s"- **Assigned to:** ${a.display}\n")
  sb ++= s"- **Created:** ${fmt(t.createdAt)}\n"
  t.updatedAt.foreach(u => sb ++= s"- **Updated:** ${fmt(Some(u))}\n")
  sb ++= s"- **Ticket id:** `${t.id}`\n\n"
  sb ++= "### Description\n\n"
  sb ++= quoted(t.description)
  sb ++= "\n\n### Conversation\n\n"
  if messages.isEmpty then sb ++= "_(no messages)_\n"
  else
    for m <- messages do
      sb ++= s"#### ${m.author.map(_.display).getOrElse("Unknown")} — ${fmt(m.createdAt)}\n\n"
      sb ++= quoted(m.text)
      sb ++= "\n"
      if m.attachments > 0 then sb ++= s"\n_${m.attachments} attachment(s) not included in this archive._\n"
      sb ++= "\n"
  sb.result()

private def parseArgs(args: Seq[String]): Config =
  def loop(rest: List[String], acc: Config): Config = rest match
    case "--society" :: v :: tail   => loop(tail, acc.copy(society = v))
    case "--token" :: v :: tail     => loop(tail, acc.copy(token = Some(v)))
    case "--token-file" :: v :: tail => loop(tail, acc.copy(tokenFile = Some(os.Path(v, os.pwd))))
    case "--out" :: v :: tail       => loop(tail, acc.copy(out = os.Path(v, os.pwd)))
    case "--status" :: v :: tail    => loop(tail, acc.copy(statuses = v))
    case unknown :: _               => sys.error(s"Unknown argument: $unknown")
    case Nil                        => acc
  loop(args.toList, Config())

private def resolveToken(cfg: Config): String =
  cfg.token
    .orElse(cfg.tokenFile.map(p => os.read(p).trim))
    .orElse(sys.env.get("BOAPPA_TOKEN"))
    .map(_.stripPrefix("Bearer ").trim)
    .filter(_.nonEmpty)
    .getOrElse(sys.error("No token: pass --token/--token-file or set BOAPPA_TOKEN"))

@main def boappaArchive(args: String*): Unit =
  val cfg = parseArgs(args)
  val backend = DefaultSyncBackend()
  try
    val client = BoappaClient(resolveToken(cfg), backend)
    val tickets = client.tickets(cfg.society, cfg.statuses).sortBy(_.number)
    System.err.println(s"Fetched ${tickets.size} tickets")

    val sb = StringBuilder()
    sb ++= s"# Boappa ticket archive\n\n"
    sb ++= s"- **Society:** `${cfg.society}`\n"
    sb ++= s"- **Exported:** ${fmt(Some(OffsetDateTime.now()))}\n"
    sb ++= s"- **Tickets:** ${tickets.size} (statuses: ${cfg.statuses})\n\n"
    sb ++= "---\n\n"

    for (t, i) <- tickets.zipWithIndex do
      System.err.println(s"[${i + 1}/${tickets.size}] #${t.number} ${t.title}")
      val messages = client.messages(cfg.society, t.id).sortBy(_.createdAt.map(_.toInstant).getOrElse(java.time.Instant.MIN))
      sb ++= renderTicket(t, messages)
      sb ++= "---\n\n"

    os.write.over(cfg.out, sb.result())
    println(s"Wrote ${cfg.out}")
  finally backend.close()
