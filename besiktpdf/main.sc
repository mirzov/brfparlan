//> using scala 3.8.1

import scala.io.Source

val phrase1 = "Nybyggnad av flerfamiljshus"
val suffix1 = "(126)"
val phrase2 = "Lägenhet"
val protocolLines = Source.fromFile("./pdfs/inspection.txt").getLines()
	.map(_.trim)
	.collect{
		case line if line.startsWith(phrase1) && line.endsWith(suffix1) =>
			line.stripPrefix(phrase1).stripSuffix(suffix1).trim.toInt
		case line if line.startsWith(phrase2) =>
			line.stripPrefix(phrase2).trim
	}
	.sliding(2, 1)
	.map(_.toList)
	.collect{
		case List(page: Int, apartment: String) => s"$apartment: $page"
	}

protocolLines.foreach(println)
