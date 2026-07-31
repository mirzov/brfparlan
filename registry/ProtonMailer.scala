import jakarta.mail.internet.InternetAddress
import jakarta.mail.internet.MimeMessage
import jakarta.mail.internet.MimeBodyPart
import jakarta.mail.internet.MimeMultipart

import java.nio.file.Files
import java.nio.file.Paths
import java.nio.file.Path
import java.util.Properties
import jakarta.mail.Message.RecipientType
import jakarta.mail.Session
import jakarta.mail.Authenticator
import jakarta.mail.PasswordAuthentication
import jakarta.mail.Transport


object ProtonMailer:
  private val smtpHost = "smtp.protonmail.ch"
  private val smtpPort = "587"
  private val username = "it|brfparlan".replace("|", "@") + ".se"
  private val subject  = "Pärlan - viktig info om ekonomisk förvaltning"
  private val password = Files.readString(Paths.get("token.txt")).trim
  private val html     = Files.readString(Paths.get("email_bredablick.html"))

  def sendEmail(to: Seq[String], recType: RecipientType, attachments: Seq[Path] = Nil): Unit =
    val props = new Properties()
    props.put("mail.smtp.auth", "true")
    props.put("mail.smtp.starttls.enable", "true")
    props.put("mail.smtp.host", smtpHost)
    props.put("mail.smtp.port", smtpPort)

    val session = Session.getInstance(
      props,
      new Authenticator():
        override protected def getPasswordAuthentication = new PasswordAuthentication(username, password)
    )

    session.setDebug(false)

    val message = new MimeMessage(session)
    val fromAddress = new InternetAddress(username)
    //fromAddress.setPersonal(fromName)
    message.setFrom(fromAddress)
    //message.setSender(fromAddress)
    to.foreach: recipient =>
      message.addRecipient(recType, new InternetAddress(recipient))
    message.setSubject(subject)
    attachments match
      case Seq() =>
        message.setContent(html, "text/html; charset=utf-8")
      case _ =>
        val multipart = new MimeMultipart()

        val textPart = new MimeBodyPart()
        textPart.setContent(html, "text/html; charset=utf-8")
        multipart.addBodyPart(textPart)

        attachments.foreach: path =>
          val attachmentPart = new MimeBodyPart()
          attachmentPart.attachFile(path.toFile)
          multipart.addBodyPart(attachmentPart)

        message.setContent(multipart)

    Transport.send(message)

end ProtonMailer
