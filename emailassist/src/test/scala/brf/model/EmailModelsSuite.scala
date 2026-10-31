package se.brfparlan.emailassist.model

import java.nio.file.{Files, Path}

import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}

import upickle.default.{read, write}

class EmailModelsSuite extends munit.FunSuite:

  private val move = MoveToFolder(
    messageId = "<abc@proton.me>",
    sourceFolder = "INBOX",
    destinationFolder = "Ekonomi/Fakturor",
    reason = "Invoice from Fastighetsservice",
    markAsRead = false
  )

  private val reply = DraftReply(
    messageId = "<abc@proton.me>",
    sourceFolder = "INBOX",
    draftContent = "Hej! Tack för ert meddelande.",
    reason = "Invoice from Fastighetsservice"
  )

  test("ActionProposal round-trips through JSON") {
    assertEquals(read[ActionProposal](write(move)), move)
    assertEquals(read[ActionProposal](write(reply)), reply)
  }

end EmailModelsSuite
