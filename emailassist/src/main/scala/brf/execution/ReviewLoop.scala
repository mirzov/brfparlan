package se.brfparlan.emailassist.execution

import scala.annotation.tailrec

import se.brfparlan.emailassist.mcp.ProposalValidator
import se.brfparlan.emailassist.model.{ActionProposal, DeleteMessage, DraftReply, MoveToFolder, StagingManager}

trait ReviewConsole:
  def println(line: String): Unit

  /** Returns None at end of input. */
  def readLine(prompt: String): Option[String]

  def editText(initial: String): Either[String, String]

enum Decision:
  case Approve, Reject, Edit, Quit

object Decision:
  def parse(input: String): Option[Decision] = input.trim.toLowerCase match
    case "y" | "yes"  => Some(Approve)
    case "n" | "no"   => Some(Reject)
    case "e" | "edit" => Some(Edit)
    case "q" | "quit" => Some(Quit)
    case _            => None

final case class ReviewSummary(executed: Int, rejected: Int, failed: Int, remaining: Int)

final class ReviewLoop(
    mailbox: Mailbox,
    staging: StagingManager,
    archive: ActionArchive,
    console: ReviewConsole
):

  private enum Outcome:
    case Executed, Rejected, Failed, Stop

  def run(unordered: Vector[ActionProposal]): ReviewSummary =
    val (drafts, others) = unordered.partition(_.isInstanceOf[DraftReply])
    val proposals = drafts ++ others
    val outcomes = proposals.iterator.zipWithIndex
      .map((p, i) => review(p, i + 1, proposals.size))
      .takeWhile(_ != Outcome.Stop)
      .toVector
    val executed = outcomes.count(_ == Outcome.Executed)
    val rejected = outcomes.count(_ == Outcome.Rejected)
    ReviewSummary(
      executed = executed,
      rejected = rejected,
      failed = outcomes.count(_ == Outcome.Failed),
      remaining = proposals.size - executed - rejected
    )

  private def review(proposal: ActionProposal, n: Int, total: Int): Outcome =
    console.println(s"\n=== Action $n of $total ===")
    console.println(s"Folder:  ${proposal.sourceFolder}")
    console.println(s"Message uid: ${proposal.uid}")
    mailbox.describe(proposal) match
      case Right(s) =>
        console.println(s"Subject: ${s.subject}")
        console.println(s"From:    ${s.from}")
        console.println(s"Date:    ${s.date}")
      case Left(err) =>
        console.println(s"Message: ${proposal.messageId} (could not load: $err)")
    decide(proposal, proposal)

  @tailrec
  private def decide(original: ActionProposal, current: ActionProposal): Outcome =
    console.println(s"Reason:  ${current.reason}")
    console.println(s"Action:  ${describeAction(current)}")
    ask() match
      case Decision.Approve => approve(original, current)
      case Decision.Reject =>
        finish(original, current, "rejected", "not executed")
        Outcome.Rejected
      case Decision.Edit => decide(original, edit(current))
      case Decision.Quit => Outcome.Stop

  @tailrec
  private def ask(): Decision =
    console.readLine("[Y]es / [N]o / [E]dit / [Q]uit: ") match
      case None => Decision.Quit
      case Some(input) =>
        Decision.parse(input) match
          case Some(d) => d
          case None =>
            console.println("Please answer y, n, e or q.")
            ask()

  private def approve(original: ActionProposal, current: ActionProposal): Outcome =
    mailbox.execute(current) match
      case Right(done) =>
        console.println(s"Done: $done")
        finish(original, current, "approved", done)
        Outcome.Executed
      case Left(err) =>
        console.println(s"Failed: $err (the action stays in the staging file)")
        Outcome.Failed

  private def finish(original: ActionProposal, current: ActionProposal, decision: String, outcome: String): Unit =
    staging.remove(original).left.foreach { err =>
      console.println(s"Warning: could not remove the action from the staging file: $err")
    }
    archive.append(decision, current, outcome).left.foreach { err =>
      console.println(s"Warning: could not archive the action: $err")
    }

  private def edit(proposal: ActionProposal): ActionProposal =
    val edited: Either[String, ActionProposal] = proposal match
      case m: MoveToFolder =>
        val destination = console
          .readLine(s"Destination folder [${m.destinationFolder}]: ")
          .map(_.trim)
          .filter(_.nonEmpty)
          .getOrElse(m.destinationFolder)
        val markAsRead =
          console.readLine(s"Mark as read? [${if m.markAsRead then "Y/n" else "y/N"}]: ").map(_.trim.toLowerCase) match
            case Some("y" | "yes") => true
            case Some("n" | "no")  => false
            case _                 => m.markAsRead
        Right(m.copy(destinationFolder = destination, markAsRead = markAsRead))
      case d: DraftReply =>
        console.editText(d.draftContent).map(text => d.copy(draftContent = text.stripTrailing))
      case x: DeleteMessage =>
        Right(x)
    edited.flatMap(ProposalValidator.validate) match
      case Right(valid) => valid
      case Left(err) =>
        console.println(s"Edit discarded: $err")
        proposal

  private def describeAction(proposal: ActionProposal): String = proposal match
    case m: MoveToFolder =>
      s"Move to ${m.destinationFolder}" + (if m.markAsRead then " and mark as read" else "")
    case d: DraftReply =>
      "Save reply draft:\n" + d.draftContent.linesIterator.map("    | " + _).mkString("\n")
    case _: DeleteMessage =>
      "Delete (move to trash)"
