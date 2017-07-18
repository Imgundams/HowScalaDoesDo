import scala.util.control.NonFatal
/**
	* Created by Administrator on 18/07/2017.
	*/
object main extends App {

	def numberToNumberAndString(aLargeNumber: String): Unit = {
		val stringOfALargeNumber = dotRemover(zeroRemover(aLargeNumber))
		val doubleOfALargeNumber = stringOfALargeNumber.toDouble
		stringPrint(stringOfALargeNumber, doubleOfALargeNumber)
		shortScaleName(stringOfALargeNumber, doubleOfALargeNumber)
		longScaleName(stringOfALargeNumber, doubleOfALargeNumber)
	}

	def shortScaleName(stringPart: String, doublePart: Double): Unit = {
		print("Short scale:")
		val shortScaleList = List("", " thousand ", " million, ", " billion, ", " trillion, ", " quadrillion, ", " quintillion, ", " sextillion, ", " septillion, ", " octillion, ", " nonillion, "," decillion, ")
		val listNumberString = stringPart.reverse.grouped(3).toList.map(_.reverse.toList.mkString)
		val output = for (i <- listNumberString.indices) yield {
			i match {
				case 0 if listNumberString(i) != "000" => "and "+zeroRemover(listNumberString(i))
				case _ if listNumberString(i) == "000" => ""
				case _ if listNumberString(i) != "000" => zeroRemover(listNumberString(i)) + shortScaleList(i)
				case _ => ""
			}
		}
		println(output.reverse.mkString)
	}

	def longScaleName(stringPart: String, doublePart: Double): Unit = {
		print("Long scale:")
		val longScaleList = List(" ", " thousand ", " million ", " milliard ", " billion ", " billiard ", " trillion ", " trilliard ", " quadrillion, ", " quadrilliard, ", " quintillion ", " quintilliard, ", " sextillion, ")
		val listNumberString = stringPart.reverse.grouped(3).toList.map(_.reverse.toList.mkString)
		val output = for (i <- listNumberString.indices) yield {
			i match {
				case 0 if listNumberString(i) != "000" => "and "+zeroRemover(listNumberString(i))
				case _ if listNumberString(i) == "000" => ""
				case _ if listNumberString(i) != "000" => zeroRemover(listNumberString(i)) + longScaleList(i)
				case _ => ""
			}
		}
		println(output.reverse.mkString)
	}

	def dotRemover(stringPart: String): String = {
		if (stringPart.contains(".")) {
			stringPart.substring(0, stringPart.indexOf("."))
		}
		else {
			stringPart
		}
	}

	def zeroRemover(stringPart: String): String = {

		if (stringPart.charAt(0) == '0') {
			zeroRemover(stringPart.substring(1))
		} else {
			stringPart
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
		try {
			val scanner = scala.io.StdIn.readLine()
			println(s"Using the value $scanner")
			val input = scanner
			numberToNumberAndString(input)
		} catch {
			case NonFatal(exc) => println(s"Error Input! $exc\n Please try again.")
				InputEntry()
			case _: Throwable => println("Big Error Here. Exiting...")
				sys.exit(0)
		}
	}

	InputEntry()

}


