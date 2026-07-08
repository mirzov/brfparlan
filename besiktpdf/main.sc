//> using scala 3.8.1
//> using dep com.lihaoyi::os-lib:0.11.8

import scala.io.Source

val phrase1 = "Nyproduktion av flerfamiljshus"
val prefix1 = "Sidan"
val suffix1 = "av 124"
val phrase2 = "LGH Brunnshögsgatan"
def protocolLines = Source.fromFile("./pdfs/GB1-Bygg.txt").getLines()
	.map(_.trim)
	.collect{
		case line if line.startsWith(phrase1) && line.endsWith(suffix1) =>
			line.stripPrefix(phrase1).trim.stripPrefix(prefix1).stripSuffix(suffix1).trim.toInt
		case line if line.startsWith(phrase2) =>
			line.stripPrefix(phrase2).trim
	}
	.sliding(2, 1)
	.map(_.toList)
	.collect{
		case List(page: Int, apartment: String) => s"$apartment: $page"
	}

//os.write.over(os.pwd / "pdfs" / "GB1-Bygg_rawPageInfo.txt",	protocolLines.mkString("\n"))

val pageRegex = """^(\S+): (\d+)$""".r

def pdfPages: Iterator[(apartment: String, startPage: Int, endPage: Int)] =
	val startPages = Source
		.fromFile("./pdfs/GB1-Bygg_pageInfo.txt")
		.getLines()
		.collect:
			case pageRegex(apartment, page) => (apartment = apartment, page = page.toInt)
	(startPages ++ Iterator("last_bogus" -> 122))
		.sliding(2, 1)
		.map: pair =>
			(apartment = pair.head.apartment, startPage = pair.head.page, endPage = pair.last.page - 1)

def qpdfCommands = pdfPages.map: apt =>
	s"""qpdf ./pdfs/GB1-Bygg.pdf --pages . 1-2,${apt.startPage}-${apt.endPage},122-124 -- ./pdfs/GB1-Bygg_byApartment/${apt.apartment}.pdf"""

def splitPdf: Unit =
	qpdfCommands.foreach: cmdTxt =>
		os.proc("bash", "-c", cmdTxt).call(stdout = os.Inherit, stderr = os.Inherit)

//qpdfCommands.foreach(println)
//splitPdf
