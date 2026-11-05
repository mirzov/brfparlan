package se.brfparlan.model.knowledge

import java.time.Instant
import java.time.LocalDate
import java.net.URL

case class Fact(
  id: String,
  topicId: Topic.Id,
  statement: String,
  validSince: Option[Instant],
  validUntil: Option[Instant],
  provenance: Option[Provenance]
)

sealed trait Provenance:
  def date: Option[LocalDate]

case class EmailSource(emailId: String, date: Option[LocalDate]) extends Provenance
case class BoardMeeting(meetingNumer: Int, date: Option[LocalDate]) extends Provenance

case class WebPage(url: URL, date: Option[LocalDate]) extends Provenance
