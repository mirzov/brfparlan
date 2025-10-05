//> using scala 3.7.1
//> using dep "com.github.tototoshi::scala-csv:2.0.0"
import scala.util.Try

import java.time.LocalDate
import java.nio.file.Paths

import com.github.tototoshi.csv._
import scala.util.Using

type Email = String
case class Person(firstName: String, lastName: String, email: Email)
case class BoappaPerson(
	email: Email,
	firstName: Option[String],
	lastName: Option[String],
	phone: Option[String]
)

case class Apartment(address: String)

case class Membership(
	person: Person,
	apartment: Apartment,
	startDate: LocalDate,
	endDate: Option[LocalDate] = None
):
	def isActiveOn(date: LocalDate): Boolean =
		(!startDate.isAfter(date)) && endDate.forall(_.isAfter(date))

	def isActiveNow: Boolean = isActiveOn(LocalDate.now())
end Membership

val root = Paths.get("/home/oleg/Documents/Pärlan")
type Row = Map[String, String]
type BoappaEntry = (apartment: Apartment, person: BoappaPerson)

def readCsv(filePath: String): Seq[Row] =
	val file = root.resolve(filePath).toFile
	Using.resource(CSVReader.open(file)):
		_.allWithHeaders()

def lookup(colName: String)(using row: Row): Option[String] =
	row.get(colName).map(_.trim).filter(_.nonEmpty)

def parseBoappaBoende(using row: Row): Option[BoappaEntry] =
	for
		email <- lookup("Epost").map(_.toLowerCase)
		address <- lookup("Bostad").flatMap(_.split(" ").headOption)
		phone = lookup("Telefon").map: raw =>
			if raw.startsWith("+") then raw else ("+46" + raw.stripPrefix("0"))
	yield (
		apartment = Apartment(address),
		person = BoappaPerson(email, lookup("Namn"), lookup("Efternamn"), phone)
	)

def parseBoappaMembers(using row: Row): Option[BoappaEntry] =
	for
		email <- lookup("Mejladress").map(_.toLowerCase)
		address <- lookup("Lägenhetsnummer (Föreningens)")
	yield (
		apartment = Apartment(address),
		person = BoappaPerson(email, lookup("Förnamn"), lookup("Efternamn"), None)
	)

def parseParlan(using row: Row): Option[Membership] =
	for
		fname <- lookup("Förnamn")
		lname <- lookup("Efternamn")
		email <- lookup("Kontakt e-mail").map(_.toLowerCase)
		address <- lookup("Lgh-nummer")
		startDateStr <- lookup("Tillträdesdatum")
		startDate <- Try(LocalDate.parse(startDateStr)).toOption
	yield Membership(
		person = Person(fname, lname, email),
		apartment = Apartment(address),
		startDate = startDate,
		endDate = lookup("Frånträdesdatum").flatMap(dateStr => Try(LocalDate.parse(dateStr)).toOption)
	)

def parseCsv[T](filePath: String)(parser: Row ?=> Option[T]): Seq[T] =
	readCsv(filePath).flatMap: row =>
		parser(using row)

lazy val parlanEntries: Seq[Membership] =
	parseCsv("Medlemsregister_pärlan.csv")(parseParlan).sortBy(_.apartment.address)

lazy val boappaEntries: Seq[BoappaEntry] =
	parseCsv("boappa_medlemslistan.csv")(parseBoappaMembers).sortBy(_.apartment.address)

lazy val boappaBoendeEntries: Seq[BoappaEntry] =
	parseCsv("boappa_boendelistan.csv")(parseBoappaBoende).sortBy(_.apartment.address)

lazy val parlanApartments: Set[Apartment] =	parlanEntries.map(_.apartment).toSet
lazy val boappaApartments: Set[Apartment] = boappaEntries.map(_.apartment).toSet

def missingInParlan: Set[Apartment] =	boappaApartments -- parlanApartments
def missingInBoappa: Set[Apartment] = parlanApartments -- boappaApartments

missingInParlan.toSeq.sortBy(_.address).foreach: apartment =>
	println(s"Missing in Medlemsregister: ${apartment.address}")

missingInBoappa.toSeq.sortBy(_.address).foreach: apartment =>
	println(s"Missing in Boappa: ${apartment.address}")

println(s"Total Pärlan apartments: ${parlanApartments.size}")

def listDifferingEmails(): Unit =
	type EmailLookup = Map[Apartment, Set[Email]]
	val parlanEmails: EmailLookup = parlanEntries
		.filter(_.isActiveNow)
		.groupMapReduce(_.apartment)(m => Set(m.person.email))(_ ++ _)
	val boappaEmails: EmailLookup = boappaEntries
		.groupMapReduce(_.apartment)(e => Set(e.person.email))(_ ++ _)
	val commonApartments = parlanApartments.intersect(boappaApartments).toSeq.sortBy(_.address)
	commonApartments.foreach: apartment =>
		val inParlan = parlanEmails(apartment)
		val inBoappa = boappaEmails(apartment)
		if inBoappa.diff(inParlan).nonEmpty then
			println(s"Apartment ${apartment.address} has differing emails:")
			println(s"  Pärlan: ${inParlan.toSeq.sorted.mkString(", ")}")
			println(s"  Boappa: ${inBoappa.toSeq.sorted.mkString(", ")}")
end listDifferingEmails

//println("Differing emails:")
//listDifferingEmails()

def exportPhoneBook(filename: String): Unit =
	val file = root.resolve(filename).toFile
	Using.resource(CSVWriter.open(file)): writer =>
		writer.writeRow(Seq("Apartment", "Email", "FirstName", "LastName", "Phone"))
		for{
			(apartment = apartment, person = person) <- boappaBoendeEntries
			phone <- person.phone
		} writer.writeRow(Seq(
			apartment.address,
			person.email,
			person.firstName.getOrElse(""),
			person.lastName.getOrElse(""),
			phone
		))
end exportPhoneBook

//exportPhoneBook("phonebook.csv")
