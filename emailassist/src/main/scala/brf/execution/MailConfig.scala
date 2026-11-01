package se.brfparlan.emailassist.execution

import java.nio.file.{Files, Path, Paths}

final case class MailConfig(
    host: String,
    port: Int,
    user: String,
    password: Option[String],
    fromAddress: String,
    draftsFolder: String,
    certFile: Option[Path],
    trashFolder: String = "Trash"
)

object MailConfig:
  val DefaultCertFile: Path = Paths.get("proton-bridge-cert.pem")

  val Usage: String =
    """Environment variables:
      |  BRF_IMAP_USER      Proton Bridge IMAP username (required)
      |  BRF_IMAP_PASSWORD  Proton Bridge IMAP password (prompted for if unset)
      |  BRF_IMAP_HOST      default 127.0.0.1
      |  BRF_IMAP_PORT      default 1143 (STARTTLS)
      |  BRF_IMAP_CERT      Bridge TLS certificate (PEM), default ./proton-bridge-cert.pem if present
      |  BRF_FROM_ADDRESS   From address for drafts, default BRF_IMAP_USER
      |  BRF_DRAFTS_FOLDER  default Drafts
      |  BRF_TRASH_FOLDER   where deleted messages are moved, default Trash""".stripMargin

  def fromEnv(env: Map[String, String]): Either[String, MailConfig] =
    def opt(name: String): Option[String] = env.get(name).map(_.trim).filter(_.nonEmpty)
    for
      user <- opt("BRF_IMAP_USER").toRight("BRF_IMAP_USER is not set")
      port <- opt("BRF_IMAP_PORT") match
        case None => Right(1143)
        case Some(p) =>
          p.toIntOption.filter(n => n > 0 && n < 65536).toRight(s"BRF_IMAP_PORT is not a valid port: $p")
      cert <- opt("BRF_IMAP_CERT") match
        case Some(p) =>
          val path = Paths.get(p)
          if Files.isReadable(path) then Right(Some(path)) else Left(s"BRF_IMAP_CERT is not readable: $p")
        case None => Right(Some(DefaultCertFile).filter(Files.isReadable))
    yield MailConfig(
      host = opt("BRF_IMAP_HOST").getOrElse("127.0.0.1"),
      port = port,
      user = user,
      password = env.get("BRF_IMAP_PASSWORD").filter(_.nonEmpty),
      fromAddress = opt("BRF_FROM_ADDRESS").getOrElse(user),
      draftsFolder = opt("BRF_DRAFTS_FOLDER").getOrElse("Drafts"),
      certFile = cert,
      trashFolder = opt("BRF_TRASH_FOLDER").getOrElse("Trash")
    )
