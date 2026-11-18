package se.brfparlan.emailassist.model

import java.nio.file.{Files, Path}

import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}

import io.circe.parser.decode
import io.circe.Encoder

class ActionProposalsSuite extends munit.FunSuite:

  private val move = MoveToFolder(
    messageId = "<abc@proton.me>",
    uid = 123L,
    sourceFolder = "INBOX",
    destinationFolder = "Ekonomi/Fakturor",
    reason = "Invoice from Fastighetsservice",
    markAsRead = false
  )

  private val reply = DraftReply(
    messageId = "<abc@proton.me>",
    uid = 223L,
    sourceFolder = "INBOX",
    draftContent = "Hej! Tack för ert meddelande.",
    reason = "Invoice from Fastighetsservice"
  )

  private def roundTrip(proposal: ActionProposal): Either[io.circe.Error, ActionProposal] =
    decode[ActionProposal](summon[Encoder[ActionProposal]](proposal).noSpaces)

  test("ActionProposal round-trips through JSON") {
    assertEquals(roundTrip(move), Right(move))
    assertEquals(roundTrip(reply), Right(reply))
  }

end ActionProposalsSuite
