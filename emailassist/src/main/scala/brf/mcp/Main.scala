package se.brfparlan.emailassist.mcp

import com.tjclp.fastmcp.{*, given}

import se.brfparlan.emailassist.model.{ActionProposal, DeleteMessage, DraftReply, MoveToFolder, StagingManager}

// Writes to stdout are the MCP protocol channel; log to stderr only.
object BrfMailMcp extends McpServerApp[Stdio, BrfMailMcp.type]:

  private val staging = StagingManager()

  @Tool(
    name = Some("propose_action"),
    description = Some(
      "Stage an email action for human review. Nothing runs until the user approves it in the terminal."
    ),
    readOnlyHint = Some(false),
    openWorldHint = Some(false)
  )
  def proposeAction(
      @Param("The action to stage: a MoveToFolder, a DraftReply or a DeleteMessage") proposal: ActionProposal
  ): String =
    ProposalValidator.validate(proposal) match
      case Left(error) => s"Error: $error"
      case Right(valid) =>
        staging.append(valid) match
          case Right(()) => "Staged for review."
          case Left(error) => s"Error: could not stage proposal: $error"

object ProposalValidator:

  def validate(proposal: ActionProposal): Either[String, ActionProposal] =
    val problems = proposal match
      case m: MoveToFolder =>
        identifier("messageId", m.messageId) ++
          identifier("sourceFolder", m.sourceFolder) ++
          identifier("destinationFolder", m.destinationFolder) ++
          text("reason", m.reason) ++
          (if m.sourceFolder == m.destinationFolder then List("sourceFolder and destinationFolder must differ")
           else Nil)
      case d: DraftReply =>
        identifier("messageId", d.messageId) ++
          identifier("sourceFolder", d.sourceFolder) ++
          text("draftContent", d.draftContent) ++
          text("reason", d.reason)
      case x: DeleteMessage =>
        identifier("messageId", x.messageId) ++
          identifier("sourceFolder", x.sourceFolder) ++
          text("reason", x.reason)
    if problems.isEmpty then Right(proposal)
    else Left(problems.mkString("rejected: ", "; ", "."))

  private def identifier(field: String, value: String): List[String] =
    if value.trim.isEmpty then List(s"$field must not be empty")
    else if value != value.trim then List(s"$field must not have leading or trailing whitespace")
    else Nil

  private def text(field: String, value: String): List[String] =
    if value.trim.isEmpty then List(s"$field must not be empty")
    else Nil
