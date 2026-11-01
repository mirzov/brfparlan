package se.brfparlan.emailassist.execution

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

import upickle.default.read

import se.brfparlan.emailassist.model.{ActionProposal, DraftReply, MoveToFolder, StagingManager}

class ReviewLoopSuite extends munit.FunSuite:

  private val move = MoveToFolder("<m1@proton.me>", 123L, "INBOX", "Ekonomi/Fakturor", "Invoice", markAsRead = false)
  private val reply = DraftReply("<m2@proton.me>", 321L, "INBOX", "Hej! Tack för ert meddelande.", "Question")

  private class FakeMailbox(failing: Set[String] = Set.empty) extends Mailbox:
    val executed = mutable.Buffer.empty[ActionProposal]
    def describe(p: ActionProposal) = Right(MessageSummary(s"Subject ${p.messageId}", "boende@example.se", "today"))
    def execute(p: ActionProposal) =
      if failing(p.messageId) then Left("boom")
      else
        executed += p
        Right("ok")

  private class ScriptedConsole(inputs: String*)(edited: String = "") extends ReviewConsole:
    private val queue = mutable.Queue(inputs*)
    val output = mutable.Buffer.empty[String]
    def println(line: String) = output += line
    def readLine(prompt: String) = Option.when(queue.nonEmpty)(queue.dequeue())
    def editText(initial: String) = Right(edited)

  private def setup(proposals: ActionProposal*): (StagingManager, ActionArchive, Path) =
    val dir = Files.createTempDirectory("review-test")
    val staging = StagingManager(dir.resolve("pending_actions.json"))
    proposals.foreach(p => assertEquals(staging.append(p), Right(())))
    val archiveFile = dir.resolve("processed_actions.jsonl")
    (staging, ActionArchive(archiveFile), archiveFile)

  private def archived(file: Path): List[ArchivedAction] =
    if !Files.exists(file) then Nil
    else Files.readAllLines(file, StandardCharsets.UTF_8).asScala.toList.map(read[ArchivedAction](_))

  test("approve executes and removes, reject removes without executing") {
    val (staging, archive, archiveFile) = setup(move, reply)
    val mailbox = FakeMailbox()
    val summary = ReviewLoop(mailbox, staging, archive, ScriptedConsole("y", "n")()).run(Vector(move, reply))
    assertEquals(summary, ReviewSummary(executed = 1, rejected = 1, failed = 0, remaining = 0))
    assertEquals(mailbox.executed.toList, List(move))
    assertEquals(staging.readAll(), Right(Vector.empty[ActionProposal]))
    assertEquals(archived(archiveFile).map(a => (a.decision, a.proposal)), List("approved" -> move, "rejected" -> reply))
  }

  test("quit and end of input leave remaining actions pending") {
    val (staging, archive, _) = setup(move, reply)
    val mailbox = FakeMailbox()
    val summary = ReviewLoop(mailbox, staging, archive, ScriptedConsole("q")()).run(Vector(move, reply))
    assertEquals(summary.remaining, 2)
    assert(mailbox.executed.isEmpty)
    assertEquals(staging.readAll(), Right(Vector(move, reply)))

    val eof = ReviewLoop(mailbox, staging, archive, ScriptedConsole("n")()).run(Vector(move, reply))
    assertEquals(eof, ReviewSummary(executed = 0, rejected = 1, failed = 0, remaining = 1))
    assertEquals(staging.readAll(), Right(Vector(reply)))
  }

  test("failed execution keeps the action and continues") {
    val (staging, archive, archiveFile) = setup(move, reply)
    val mailbox = FakeMailbox(failing = Set(move.messageId))
    val summary = ReviewLoop(mailbox, staging, archive, ScriptedConsole("y", "y")()).run(Vector(move, reply))
    assertEquals(summary, ReviewSummary(executed = 1, rejected = 0, failed = 1, remaining = 1))
    assertEquals(staging.readAll(), Right(Vector(move)))
    assertEquals(archived(archiveFile).map(_.proposal), List(reply))
  }

  test("invalid input re-prompts") {
    val (staging, archive, _) = setup(move)
    val console = ScriptedConsole("maybe", "y")()
    val summary = ReviewLoop(FakeMailbox(), staging, archive, console).run(Vector(move))
    assertEquals(summary.executed, 1)
    assert(console.output.contains("Please answer y, n, e or q."))
  }

  test("edited move is executed and the original is removed") {
    val (staging, archive, archiveFile) = setup(move)
    val mailbox = FakeMailbox()
    val console = ScriptedConsole("e", "Ekonomi/Kvitton", "y", "y")()
    ReviewLoop(mailbox, staging, archive, console).run(Vector(move))
    val expected = move.copy(destinationFolder = "Ekonomi/Kvitton", markAsRead = true)
    assertEquals(mailbox.executed.toList, List(expected))
    assertEquals(staging.readAll(), Right(Vector.empty[ActionProposal]))
    assertEquals(archived(archiveFile).map(_.proposal), List(expected))
  }

  test("edited draft uses the editor text and invalid edits are discarded") {
    val (staging, archive, _) = setup(reply, reply.copy(messageId = "<m3@proton.me>"))
    val mailbox = FakeMailbox()
    ReviewLoop(mailbox, staging, archive, ScriptedConsole("e", "y")("Hej igen!\n")).run(Vector(reply))
    assertEquals(mailbox.executed.toList, List(reply.copy(draftContent = "Hej igen!")))

    val console = ScriptedConsole("e", "y")("   \n")
    val other = reply.copy(messageId = "<m3@proton.me>")
    ReviewLoop(mailbox, staging, archive, console).run(Vector(other))
    assertEquals(mailbox.executed.last, other)
    assert(console.output.exists(_.startsWith("Edit discarded")))
  }

  test("Decision.parse accepts short and long forms") {
    assertEquals(Decision.parse(" Y "), Some(Decision.Approve))
    assertEquals(Decision.parse("no"), Some(Decision.Reject))
    assertEquals(Decision.parse("edit"), Some(Decision.Edit))
    assertEquals(Decision.parse("Q"), Some(Decision.Quit))
    assertEquals(Decision.parse("x"), None)
  }
end ReviewLoopSuite
