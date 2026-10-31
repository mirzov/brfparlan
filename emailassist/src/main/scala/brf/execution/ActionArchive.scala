package se.brfparlan.emailassist.execution

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}
import java.time.Instant

import upickle.default.{ReadWriter, write}

import se.brfparlan.emailassist.model.ActionProposal

final case class ArchivedAction(
    processedAt: String,
    decision: String,
    proposal: ActionProposal,
    outcome: String
) derives ReadWriter

/** Append-only JSON Lines log of reviewed actions. */
final class ActionArchive(file: Path):

  def append(decision: String, proposal: ActionProposal, outcome: String): Either[String, Unit] =
    val entry = ArchivedAction(Instant.now().toString, decision, proposal, outcome)
    try
      Files.createDirectories(file.toAbsolutePath.getParent)
      Files.writeString(
        file,
        write(entry) + "\n",
        StandardCharsets.UTF_8,
        StandardOpenOption.CREATE,
        StandardOpenOption.APPEND
      )
      Right(())
    catch case e: IOException => Left(s"failed to write $file: ${e.getMessage}")
