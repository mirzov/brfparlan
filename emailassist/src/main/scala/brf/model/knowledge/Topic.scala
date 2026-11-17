package se.brfparlan.model.knowledge

import java.nio.file.Files
import java.nio.file.Path
import java.text.Normalizer
import io.circe.{Codec, Decoder, Encoder}
import io.circe.parser.decode
import io.circe.syntax.*


trait Topic:
  def id: Topic.Id
  def name: String
  def description: String
  def parents: Seq[Topic.Id]


object Topic:
  opaque type Id <: String = String

  private case class TopicImpl(
    id: Topic.Id,
    name: String,
    description: String,
    parents: Seq[Topic.Id]
  ) extends Topic derives Codec.AsObject

  private def asImpl(topic: Topic): TopicImpl = topic match
    case impl: TopicImpl => impl
    case _ => TopicImpl(
      id = topic.id,
      name = topic.name,
      description = topic.description,
      parents = topic.parents
    )

  private val validIdPattern = "^[a-z]+[a-z0-9_]*$".r

  /** Creates a Topic.Id from a String by stripping whitespace and diacritics */
  def makeId(name: String): Id = toAscii(name)

  def parseId(s: String): Option[Id] = if validIdPattern.matches(s) then Some(s) else None

  def parseFile(path: Path): Seq[Topic] =
    decode[Seq[TopicImpl]](Files.readString(path)).fold(throw _, identity)

  def writeFile(topics: Seq[Topic], path: Path): Unit =
    Files.writeString(path, topics.map(asImpl).asJson.spaces2)

end Topic

given Codec[Topic.Id] = Codec.from(
  Decoder.decodeString.emap(str => Topic.parseId(str).toRight(s"Invalid format for Topic id: $str")),
  Encoder.encodeString.contramap[Topic.Id](identity)
)


private val nonDecomposableCharacters: Map[Char, String] = Map(
  'ø' -> "o",
  'æ' -> "ae",
  'œ' -> "oe",
  'ß' -> "ss",
  'đ' -> "d",
  'ð' -> "d",
  'þ' -> "th",
  'ł' -> "l",
  'ħ' -> "h",
  'ı' -> "i"
)

def toAscii(str: String): String =
  val lowered = str.trim.toLowerCase
  val mapped = lowered.flatMap(c => nonDecomposableCharacters.getOrElse(c, c.toString))
  Normalizer
    .normalize(mapped, Normalizer.Form.NFD)
    .replaceAll("\\p{M}+", "")
    .replaceAll("\\s+", "_")
