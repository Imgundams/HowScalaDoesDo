import scala.util.{Failure, Success, Try}

// Created by DRain on 18/07/2017.
object Main extends App {
	def numberToNumberAndString(aLargeNumber: String): Unit = {
		val shortScaleList = List("", " thousand ", " million, ", " billion, ", " trillion, ", " quadrillion, ", " quintillion, ", " sextillion, ", " septillion, ", " octillion, ", " nonillion, ", " decillion, ")
		val longScaleList = List(" ", " thousand ", " million ", " milliard ", " billion ", " billiard ", " trillion ", " trilliard ", " quadrillion, ", " quadrilliard, ", " quintillion ", " quintilliard, ", " sextillion, ")
		val stringOfALargeNumber = dotRemover(zeroRemover(aLargeNumber))
		val doubleOfALargeNumber = stringOfALargeNumber.toDouble
		stringPrint(stringOfALargeNumber, doubleOfALargeNumber)
		print("Short scale:")
		scaleName(stringOfALargeNumber, doubleOfALargeNumber, shortScaleList)
		print("Long scale:")
		scaleName(stringOfALargeNumber, doubleOfALargeNumber, longScaleList)
	}

	def scaleName(stringPart: String, doublePart: Double, format: List[String]): Unit = {
		val listNumberString = stringPart.reverse.grouped(3).toList.map(_.reverse.toList.mkString)
		val output = for (i <- listNumberString.indices) yield {
			i match {
				case 0 if listNumberString(i) != "000" => "and " + zeroRemover(listNumberString(i))
				case _ if listNumberString(i) == "000" => ""
				case _ if listNumberString(i) != "000" => zeroRemover(listNumberString(i)) + format(i)
				case _ => ""
			}
		}
		println(output.reverse.mkString)
	}

	def dotRemover(stringPart: String): String = {
		stringPart match {
			case _ if stringPart.contains(".") => stringPart.substring(0, stringPart.indexOf("."))
			case _ => stringPart
		}
	}

	def zeroRemover(stringPart: String): String = {
		stringPart match {
			case _ if stringPart.charAt(0) == '0' => zeroRemover(stringPart.substring(1))
			case _ => stringPart
		}
	}

	def stringPrint(stringPart: String, doublePart: Double): Unit = {
		print("The Length of the Number is :")
		println(stringPart.length)
		print("The Number used as a String is :")
		println(stringPart)
		print("The Number as a Double is :")
		println(doublePart)
	}

	def InputEntry(): Unit = {
		println("Enter a Number to parse:\n")
		Try {
			val scanner = scala.io.StdIn.readLine()
			println(s"Using the value $scanner")
			val input = scanner
			numberToNumberAndString(input)
		} match {
			case Success(trying) => println("Done!")
			case Failure(trying) =>
				println(s"Error Input! $trying\n Please try again.")
				InputEntry()
		}
	}

	InputEntry()
}