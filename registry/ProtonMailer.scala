import jakarta.mail.*
import jakarta.mail.internet.InternetAddress
import jakarta.mail.internet.MimeMessage
import jakarta.mail.internet.MimeBodyPart
import jakarta.mail.internet.MimeMultipart

import java.nio.file.Files
import java.nio.file.Paths
import java.nio.file.Path
import java.util.Properties


object ProtonMailer:
  private val smtpHost = "smtp.protonmail.ch"
  private val smtpPort = "587"
  private val username = "besikt|brfparlan".replace("|", "@") + ".se"
  private val fromName = "Tvåårsbesiktning Brf Pärlan"
  private val password = Files.readString(Paths.get("token.txt")).trim

  def sendEmail(to: String, subject: String, body: String, attachment: Option[Path]): Unit =
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
    message.addRecipient(Message.RecipientType.TO, new InternetAddress(to))
    message.setSubject(subject)
    attachment match
      case Some(path) =>
        val multipart = new MimeMultipart()

        val textPart = new MimeBodyPart()
        textPart.setText(body)
        multipart.addBodyPart(textPart)

        val attachmentPart = new MimeBodyPart()
        attachmentPart.attachFile(path.toFile)
        multipart.addBodyPart(attachmentPart)

        message.setContent(multipart)
      case None =>
        message.setText(body)

    Transport.send(message)

end ProtonMailer
