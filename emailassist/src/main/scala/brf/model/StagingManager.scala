package se.brfparlan.emailassist.model

import java.io.IOException
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardCopyOption, StandardOpenOption}

import scala.util.control.NonFatal

import upickle.default.{read, write}

object StagingManager:
  val DefaultFile: Path = Paths.get(".brf", "pending_actions.json")

  // Serializes access between threads in this JVM; the file lock covers other processes.
  private val jvmLock = new Object

final class StagingManager(file: Path = StagingManager.DefaultFile):

  import StagingManager.jvmLock

  def readAll(): Either[String, Vector[ActionProposal]] =
    jvmLock.synchronized {
      withFileLock(readUnsafe())
    }

  def append(proposal: ActionProposal): Either[String, Unit] =
    jvmLock.synchronized {
      withFileLock {
        for
          existing <- readUnsafe()
          _ <- writeUnsafe(existing :+ proposal)
        yield ()
      }
    }

  /** Removes the first entry equal to `proposal`; entries appended meanwhile are kept. */
  def remove(proposal: ActionProposal): Either[String, Unit] =
    jvmLock.synchronized {
      withFileLock {
        readUnsafe().flatMap { existing =>
          existing.indexOf(proposal) match
            case -1 => Left(s"proposal is no longer present in $file")
            case i  => writeUnsafe(existing.patch(i, Nil, 1))
        }
      }
    }

  private def readUnsafe(): Either[String, Vector[ActionProposal]] =
    if !Files.exists(file) then Right(Vector.empty)
    else
      try
        val text = Files.readString(file, StandardCharsets.UTF_8)
        if text.isBlank then Right(Vector.empty)
        else Right(read[Vector[ActionProposal]](text))
      catch
        case NonFatal(e) => Left(s"failed to read $file: ${e.getMessage}")

  private def writeUnsafe(proposals: Vector[ActionProposal]): Either[String, Unit] =
    try
      val parent = file.toAbsolutePath.getParent
      Files.createDirectories(parent)
      val text = write(proposals, indent = 2)
      // Write to a temp file and move it into place so readers never see a partial file.
      val tmp = file.resolveSibling(file.getFileName.toString + ".tmp")
      Files.writeString(tmp, text, StandardCharsets.UTF_8)
      Files.move(tmp, file, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
      Right(())
    catch
      case e: IOException => Left(s"failed to write $file: ${e.getMessage}")

  private def withFileLock[A](body: => Either[String, A]): Either[String, A] =
    try
      val parent = file.toAbsolutePath.getParent
      Files.createDirectories(parent)
      val lockPath = file.resolveSibling(file.getFileName.toString + ".lock")
      val channel = FileChannel.open(
        lockPath,
        StandardOpenOption.CREATE,
        StandardOpenOption.WRITE
      )
      try
        val lock = channel.lock()
        try body
        finally lock.release()
      finally channel.close()
    catch
      case e: IOException => Left(s"failed to lock $file: ${e.getMessage}")
