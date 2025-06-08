//> using scala 3.7.1
//> using dep "com.github.tototoshi::scala-csv:2.0.0"

import java.time.LocalDate
import java.nio.file.Paths

import com.github.tototoshi.csv._
import scala.util.Using

case class Person(firstName: String, lastName: String, email: String)
case class Apartment(address: String)

case class Membership(
  person: Person,
  apartment: Apartment,
  startDate: LocalDate
)

val root = Paths.get("/home/oleg/Documents/Pärlan")
type Row = Map[String, String]

def readCsv(filePath: String): Seq[Map[String, String]] =
  val file = root.resolve(filePath).toFile
  Using.resource(CSVReader.open(file)):
    _.allWithHeaders()

def lookup(colName: String)(using row: Map[String, String]): Option[String] =
  row.get(colName).map(_.trim).filter(_.nonEmpty)

def parseBoappa(using row: Map[String, String]): Option[(apartment: Apartment, person: Person)] =
  for
    fname <- lookup("Namn")
    lname <- lookup("Efternamn")
    email <- lookup("Epost").map(_.toLowerCase)
    address <- lookup("Bostad").flatMap(_.split(" ").headOption)
  yield (
    apartment = Apartment(address),
    person = Person(fname, lname, email)
  )

def parseCsv[T](filePath: String)(parser: Row ?=> Option[T]): Seq[T] =
  readCsv(filePath).flatMap: row =>
    parser(using row)

parseCsv("boappa_boendelistan.csv")(parseBoappa).sortBy(_.apartment.address).foreach:
  bapp =>
    println(s"${bapp.apartment.address} <- ${bapp.person.email}")
