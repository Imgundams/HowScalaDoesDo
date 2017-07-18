/**
	* Created by Administrator on 18/07/2017.
	*/
object main extends App {

	def numberToNumberAndString(aLargeNumber: String): Unit = {
		val numberNoLeadZeros = zeroRemover(aLargeNumber)
		val stringOfALargeNumber = dotRemover(numberNoLeadZeros)
		val doubleOfALargeNumber = stringOfALargeNumber.toDouble
		stringPrint(stringOfALargeNumber, doubleOfALargeNumber)
		shortScaleName(stringOfALargeNumber, doubleOfALargeNumber)
		longScaleName(stringOfALargeNumber, doubleOfALargeNumber)
	}

	def shortScaleName(stringPart: String, doublePart: Double): Unit = {
		print("Short scale:")
		val english = List("", " thousand and ", " million, ", " billion, ", " trillion, ", " quadrillion, ", " quintillion, ", " sextillion, ", " septillion, ", " octillion, ", " nonillion, ")
		val listNumberString = stringPart.reverse.grouped(3).toList.map(_.reverse.toList.mkString)
		val output = for (i <- listNumberString.indices) yield {
			if (listNumberString(i)== "000"){
				english(i)
			}
			else {
				listNumberString(i) + english(i)
			}
		}
		println(output.reverse.mkString)

	}

	def longScaleName(stringPart: String, doublePart: Double): Unit = {
		print("Long scale:")
		val notEnglish = List(" ", " thousand and ", " million ", " milliard ", " billion ", " billiard ", " trillion ", " trilliard ", " quadrillion, ", " quadrilliard, ", " quintillion ", " quintilliard, ", " sextillion, ")
		val listNumberString = stringPart.reverse.grouped(3).toList.map(_.reverse.toList.mkString)
		val output = for (i <- listNumberString.indices) yield {
			if (listNumberString(i)== "000"){
				notEnglish(i)
			}
			else {
				listNumberString(i) + notEnglish(i)
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
			case e: Exception => println(s"Error Input! $e\n Please try again.")
				InputEntry()
			case _: Throwable => println("Big Error Here. Exiting...")
				sys.exit(0)
		}
	}

	InputEntry()

}


