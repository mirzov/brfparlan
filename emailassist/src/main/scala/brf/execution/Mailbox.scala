package se.brfparlan.emailassist.execution

import java.nio.file.{Files, Path}
import java.security.KeyStore
import java.security.cert.CertificateFactory
import java.util.Properties
import javax.net.ssl.{SSLContext, SSLSocketFactory, TrustManagerFactory}

import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.control.NonFatal

import jakarta.mail.internet.{InternetAddress, MimeMessage}
import jakarta.mail.search.MessageIDTerm
import jakarta.mail.{Flags, Folder, Message, MessagingException, Session, Store}
import org.eclipse.angus.mail.imap.IMAPFolder

import se.brfparlan.emailassist.model.{ActionProposal, DeleteMessage, DraftReply, MoveToFolder}
import java.security.cert.Certificate

final case class MessageSummary(subject: String, from: String, date: String)

trait Mailbox:
  def describe(proposal: ActionProposal): Either[String, MessageSummary]

  /** Performs the action and returns a short description of what was done. */
  def execute(proposal: ActionProposal): Either[String, String]

final class ImapMailbox private (store: Store, config: MailConfig) extends Mailbox, AutoCloseable:

  def describe(proposal: ActionProposal): Either[String, MessageSummary] =
    withOpenFolder(proposal.sourceFolder, Folder.READ_ONLY) { folder =>
      find(folder, proposal.messageId).flatMap { msg =>
        attempt {
          MessageSummary(
            subject = Option(msg.getSubject).getOrElse("(no subject)"),
            from = Option(msg.getFrom).toList.flatten.map(_.toString).mkString(", "),
            date = Option(msg.getSentDate).map(_.toString).getOrElse("(unknown)")
          )
        }
      }
    }

  def execute(proposal: ActionProposal): Either[String, String] = proposal match
    case m: MoveToFolder => move(m)
    case d: DraftReply   => saveDraft(d)
    case x: DeleteMessage => delete(x)

  def close(): Unit =
    try store.close()
    catch case NonFatal(_) => ()

  private def move(m: MoveToFolder): Either[String, String] =
    withOpenFolder(m.sourceFolder, Folder.READ_WRITE) { source =>
      for
        msg <- find(source, m.messageId)
        dest <- resolveFolder(m.destinationFolder)
        _ <- attempt {
          if m.markAsRead then msg.setFlag(Flags.Flag.SEEN, true)
          source match
            case imap: IMAPFolder => imap.moveMessages(Array(msg), dest)
            case other => throw MessagingException(s"unexpected folder type ${other.getClass.getName}")
        }
      yield s"moved to '${dest.getFullName}'"
    }

  // Deletion is recoverable: the message goes to the trash folder rather than being expunged.
  private def delete(x: DeleteMessage): Either[String, String] =
    if ImapMailbox.sameFolder(x.sourceFolder, config.trashFolder) then
      Left(s"message is already in the trash folder '${config.trashFolder}'")
    else
      withOpenFolder(x.sourceFolder, Folder.READ_WRITE) { source =>
        for
          msg <- find(source, x.messageId)
          trash <- resolveFolder(config.trashFolder)
          _ <- attempt {
            source match
              case imap: IMAPFolder => imap.moveMessages(Array(msg), trash)
              case other => throw MessagingException(s"unexpected folder type ${other.getClass.getName}")
          }
        yield s"moved to trash '${trash.getFullName}'"
      }

  private def saveDraft(d: DraftReply): Either[String, String] =
    withOpenFolder(d.sourceFolder, Folder.READ_ONLY) { source =>
      for
        original <- find(source, d.messageId)
        drafts <- resolveFolder(config.draftsFolder)
        _ <- attempt {
          // Don't flag the original as answered: this is only a draft.
          val reply = original.asInstanceOf[MimeMessage].reply(false, false).asInstanceOf[MimeMessage]
          reply.setFrom(InternetAddress(config.fromAddress))
          reply.setText(d.draftContent, "UTF-8")
          reply.setFlag(Flags.Flag.DRAFT, true)
          reply.saveChanges()
          drafts.appendMessages(Array(reply))
        }
      yield s"reply draft saved in '${drafts.getFullName}'"
    }

  private def withOpenFolder[A](name: String, mode: Int)(body: Folder => Either[String, A]): Either[String, A] =
    resolveFolder(name).flatMap { folder =>
      attempt(folder.open(mode)).flatMap { _ =>
        try body(folder)
        finally
          try folder.close(false)
          catch case NonFatal(_) => ()
      }
    }

  // Proton Bridge exposes user folders under "Folders/".
  private def resolveFolder(name: String): Either[String, Folder] =
    attempt {
      List(name, s"Folders/$name").iterator.map(store.getFolder).find(_.exists())
    }.flatMap(_.toRight(s"folder '$name' does not exist"))

  private def find(folder: Folder, messageId: String): Either[String, Message] =
    val wanted = ImapMailbox.normalizeId(messageId)
    attempt {
      folder.search(MessageIDTerm(messageId)).filter { msg =>
        Option(msg.getHeader("Message-ID")).toList.flatten.exists(h => ImapMailbox.normalizeId(h) == wanted)
      }
    }.flatMap {
      case Array(msg) => Right(msg)
      case Array()    => Left(s"message $messageId not found in '${folder.getFullName}'")
      case many       => Left(s"${many.length} messages match $messageId in '${folder.getFullName}'; refusing to act")
    }

  private def attempt[A](body: => A): Either[String, A] =
    try Right(body)
    catch case e: MessagingException => Left(Option(e.getMessage).getOrElse(e.getClass.getSimpleName))

object ImapMailbox:

  def connect(config: MailConfig, password: String): Either[String, ImapMailbox] =
    try
      val props = Properties()
      props.put("mail.store.protocol", "imap")
      props.put("mail.imap.host", config.host)
      props.put("mail.imap.port", config.port.toString)
      props.put("mail.imap.starttls.enable", "true")
      props.put("mail.imap.starttls.required", "true")
      props.put("mail.imap.ssl.checkserveridentity", "true")
      props.put("mail.imap.connectiontimeout", "15000")
      props.put("mail.imap.timeout", "60000")
      config.certFile.foreach(cert => props.put("mail.imap.ssl.socketFactory", socketFactory(cert)))
      val store = Session.getInstance(props).getStore("imap")
      store.connect(config.host, config.port, config.user, password)
      Right(ImapMailbox(store, config))
    catch case NonFatal(e) => Left(s"could not connect to ${config.host}:${config.port}: ${e.getMessage}")

  def sameFolder(a: String, b: String): Boolean =
    a.stripPrefix("Folders/") == b.stripPrefix("Folders/")

  def normalizeId(id: String): String = id.trim.stripPrefix("<").stripSuffix(">")

  /** Trusts only the certificates in `certFile`, e.g. the Proton Bridge self-signed certificate. */
  private def socketFactory(certFile: Path): SSLSocketFactory =
    val certs: IndexedSeq[Certificate] = Using.resource(Files.newInputStream(certFile)) { in =>
      CertificateFactory.getInstance("X.509").generateCertificates(in).asScala.toIndexedSeq
    }
    val keyStore = KeyStore.getInstance(KeyStore.getDefaultType)
    keyStore.load(null, null)
    certs.zipWithIndex.foreach((cert, i) => keyStore.setCertificateEntry(s"bridge-$i", cert))
    val tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
    tmf.init(keyStore)
    val ctx = SSLContext.getInstance("TLS")
    ctx.init(null, tmf.getTrustManagers, null)
    ctx.getSocketFactory
