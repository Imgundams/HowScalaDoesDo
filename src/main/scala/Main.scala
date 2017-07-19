import scala.util.{Failure, Success, Try}

// Created by DRain on 18/07/2017.
object Main extends App {
	def numberToNumberAndString(aLargeNumber: String): String = {
		val shortScaleList = List("", " thousand", " million,", " billion,", " trillion,", " quadrillion,",
			" quintillion,", " sextillion,", " septillion,", " octillio,n", " nonillion,", " decillion,")
		val longScaleList = List(" ", " thousand", " million,", " milliard,", " billion,", " billiard,",
			" trillion,", " trilliard,", " quadrillion,", " quadrilliard,", " quintillion,", " quintilliard,", " sextillion,")
		val stringOfALargeNumber = dotRemover(zeroRemover(aLargeNumber))
		val doubleOfALargeNumber = stringOfALargeNumber.toDouble
		val output = "Short scale:" + scaleName(stringOfALargeNumber, doubleOfALargeNumber, shortScaleList) +
			"\nLong scale:" + scaleName(stringOfALargeNumber, doubleOfALargeNumber, longScaleList)
		output
	}

	def scaleName(stringPart: String, doublePart: Double, format: List[String]): String = {
		val listNumberString = stringPart.reverse.grouped(3).toList.map(_.reverse.toList.mkString)
		val output = for (i <- listNumberString.indices) yield {
			i match {
				case 0 if listNumberString(i) != "000" => " and " + zeroRemover(listNumberString(i))
				case _ if listNumberString(i) == "000" => ""
				case _ if listNumberString(i) != "000" => " " + zeroRemover(listNumberString(i)) + format(i)
				case _ => ""
			}
		}
		output.reverse.mkString
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

	def inputEntry(input: String): String = {
		//	print("\nEnter a Number to parse:")
		Try {
			//			val scanner = scala.io.StdIn.readLine()
			//			print(s"\nUsing the value $scanner")
			//			val input = scanner
			val output = numberToNumberAndString(input)
		} match {
			case Success(trying) =>
				val output = numberToNumberAndString(input)
				output
			case Failure(trying) =>
				//		print(s"\nError Input! $trying\n Please try again.")
				//		inputEntry(input)
				"Bad Input!"
		}
	}

	print(inputEntry("12345678901234567890"))
}