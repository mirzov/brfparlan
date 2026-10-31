package se.brfparlan.emailassist.execution

import java.nio.file.Files

class MailConfigSuite extends munit.FunSuite:

  test("defaults are applied when only the user is set") {
    val config = MailConfig.fromEnv(Map("BRF_IMAP_USER" -> "styrelsen@proton.me", "BRF_IMAP_CERT" -> ""))
    assertEquals(config.map(c => (c.host, c.port, c.fromAddress, c.draftsFolder, c.password)),
      Right(("127.0.0.1", 1143, "styrelsen@proton.me", "Drafts", None)))
  }

  test("missing user, bad port and unreadable cert are rejected") {
    assert(MailConfig.fromEnv(Map.empty).isLeft)
    assert(MailConfig.fromEnv(Map("BRF_IMAP_USER" -> "u", "BRF_IMAP_PORT" -> "abc")).isLeft)
    assert(MailConfig.fromEnv(Map("BRF_IMAP_USER" -> "u", "BRF_IMAP_CERT" -> "/no/such/cert.pem")).isLeft)
  }

  test("explicit values override defaults") {
    val cert = Files.createTempFile("bridge", ".pem")
    val env = Map(
      "BRF_IMAP_USER" -> "u",
      "BRF_IMAP_PASSWORD" -> "secret",
      "BRF_IMAP_HOST" -> "localhost",
      "BRF_IMAP_PORT" -> "1993",
      "BRF_IMAP_CERT" -> cert.toString,
      "BRF_FROM_ADDRESS" -> "info@brf.se",
      "BRF_DRAFTS_FOLDER" -> "Utkast"
    )
    assertEquals(
      MailConfig.fromEnv(env),
      Right(MailConfig("localhost", 1993, "u", Some("secret"), "info@brf.se", "Utkast", Some(cert)))
    )
  }
end MailConfigSuite
