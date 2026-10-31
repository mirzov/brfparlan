package se.brfparlan.emailassist.model

import upickle.default.ReadWriter

sealed trait ActionProposal derives ReadWriter:
  def messageId: String
  def sourceFolder: String
  def reason: String

final case class MoveToFolder(
    messageId: String,
    sourceFolder: String,
    destinationFolder: String,
    reason: String,
    markAsRead: Boolean
) extends ActionProposal

final case class DraftReply(
    messageId: String,
    sourceFolder: String,
    draftContent: String,
    reason: String
) extends ActionProposal
