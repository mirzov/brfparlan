package se.brfparlan.emailassist.execution

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import scala.jdk.CollectionConverters.*

import se.brfparlan.emailassist.model.StagingManager

/** Interactive review of `.brf/pending_actions.json`; approved actions are executed via Proton Bridge IMAP. */
object ExecutionRunner:

  def main(args: Array[String]): Unit =
    run(TerminalConsole()) match
      case Right(()) => ()
      case Left(err) =>
        System.err.println(s"Error: $err")
        sys.exit(1)

  private def run(console: ReviewConsole): Either[String, Unit] =
    val staging = StagingManager()
    val archive = ActionArchive(StagingManager.DefaultFile.resolveSibling("processed_actions.jsonl"))
    staging.readAll().flatMap { proposals =>
      if proposals.isEmpty then Right(console.println("No pending actions."))
      else
        for
          config <- MailConfig.fromEnv(sys.env).left.map(err => s"$err\n${MailConfig.Usage}")
          password <- config.password.orElse(promptPassword(config.user)).toRight("no IMAP password given")
          mailbox <- ImapMailbox.connect(config, password)
        yield
          try
            val s = ReviewLoop(mailbox, staging, archive, console).run(proposals)
            console.println(
              s"\nExecuted: ${s.executed}, rejected: ${s.rejected}, failed: ${s.failed}, still pending: ${s.remaining}"
            )
          finally mailbox.close()
    }

  private def promptPassword(user: String): Option[String] =
    Option(System.console())
      .flatMap(c => Option(c.readPassword("IMAP password for %s: ", user)))
      .map(String(_))
      .filter(_.nonEmpty)

final class TerminalConsole extends ReviewConsole:

  def println(line: String): Unit = Console.out.println(line)

  def readLine(prompt: String): Option[String] =
    Console.out.print(prompt)
    Console.out.flush()
    Option(scala.io.StdIn.readLine())

  def editText(initial: String): Either[String, String] =
    val editor = sys.env.get("VISUAL").orElse(sys.env.get("EDITOR")).map(_.trim).filter(_.nonEmpty).getOrElse("nano")
    val tmp = Files.createTempFile("brf-draft-", ".txt")
    try
      Files.writeString(tmp, initial, StandardCharsets.UTF_8)
      val command = (editor.split("\\s+").toList :+ tmp.toString).asJava
      val exit = ProcessBuilder(command).inheritIO().start().waitFor()
      if exit != 0 then Left(s"editor '$editor' exited with code $exit")
      else Right(Files.readString(tmp, StandardCharsets.UTF_8))
    catch case e: IOException => Left(s"could not run editor '$editor': ${e.getMessage}")
    finally Files.deleteIfExists(tmp)
