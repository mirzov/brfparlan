import jakarta.mail.*
import jakarta.mail.internet.InternetAddress
import jakarta.mail.internet.MimeMessage

import java.nio.file.Files
import java.nio.file.Paths
import java.util.Properties


object ProtonMailer:
  private val smtpHost = "smtp.protonmail.ch"
  private val smtpPort = "587"
  private val username = "besikt|brfparlan".replace("|", "@") + ".se"
  private val fromName = "Tvåårsbesiktning Brf Pärlan"
  private val password = Files.readString(Paths.get("token.txt")).trim

  def sendEmail(to: String, subject: String, body: String): Unit =
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

    session.setDebug(true)

    val message = new MimeMessage(session)
    message.setFrom(new InternetAddress(username))
    message.addRecipient(Message.RecipientType.TO, new InternetAddress(to))
    message.setSubject(subject)
    message.setText(body)

    Transport.send(message)

end ProtonMailer
