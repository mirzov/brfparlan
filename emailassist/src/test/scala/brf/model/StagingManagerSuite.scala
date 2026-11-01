package se.brfparlan.emailassist.model

import java.nio.file.{Files, Path}
import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}
import upickle.default.{read, write}

class StagingManagerSuite extends munit.FunSuite:

  private val move = MoveToFolder(
    "<m1@proton.me>",
    123L,
    "INBOX",
    "Ansökningar/Andrahandsuthyrning",
    "Application for sublet",
    markAsRead = false
  )

  private def tempFile(): Path =
    val dir = Files.createTempDirectory("staging-test")
    dir.resolve("nested").resolve("pending_actions.json")

  test("missing file reads as empty") {
    val staging = StagingManager(tempFile())
    assertEquals(staging.readAll(), Right(Vector.empty[ActionProposal]))
  }

  test("append persists proposals in order and creates the directory") {
    val file = tempFile()
    val staging = StagingManager(file)
    val second = move.copy(messageId = "<m2@proton.me>", markAsRead = true)
    assertEquals(staging.append(move), Right(()))
    assertEquals(staging.append(second), Right(()))
    assertEquals(staging.readAll(), Right(Vector(move, second)))
    assertEquals(StagingManager(file).readAll(), Right(Vector(move, second)))
    assert(!Files.exists(file.resolveSibling("pending_actions.json.tmp")))
  }

  test("concurrent appends from many threads are all kept") {
    val staging = StagingManager(tempFile())
    given ExecutionContext = ExecutionContext.global
    val futures = (1 to 50).map { i =>
      Future(staging.append(move.copy(messageId = s"<m$i@proton.me>")))
    }
    Await.result(Future.sequence(futures), 30.seconds).foreach(r => assertEquals(r, Right(())))
    assertEquals(staging.readAll().map(_.size), Right(50))
  }

  test("remove deletes only the first matching entry") {
    val staging = StagingManager(tempFile())
    val other = move.copy(messageId = "<m2@proton.me>")
    Seq(move, other, move).foreach(p => assertEquals(staging.append(p), Right(())))
    assertEquals(staging.remove(move), Right(()))
    assertEquals(staging.readAll(), Right(Vector(other, move)))
    assertEquals(staging.remove(move), Right(()))
    assert(staging.remove(move).isLeft)
    assertEquals(staging.readAll(), Right(Vector(other)))
  }

  test("corrupt file produces a Left instead of throwing") {
    val file = tempFile()
    Files.createDirectories(file.getParent)
    Files.writeString(file, "{not json")
    assert(StagingManager(file).readAll().isLeft)
  }

  test("invalid entry makes the whole read fail") {
    val file = tempFile()
    Files.createDirectories(file.getParent)
    Files.writeString(file, """[{"actionType":"move"}]""")
    assert(StagingManager(file).readAll().isLeft)
  }
end StagingManagerSuite
