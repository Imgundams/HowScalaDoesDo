/*
Created by DRain on 18/07/2017.
*/

import scala.util.{Failure, Success, Try}

class NumberToScaleName {
  val shortScaleList = List("", " thousand", " million,", " billion,", " trillion,", " quadrillion,",
    " quintillion,", " sextillion,", " septillion,", " octillio,n", " nonillion,", " decillion,", " undecillion,")
  val longScaleList = List(" ", " thousand", " million,", " milliard,", " billion,", " billiard,",
    " trillion,", " trilliard,", " quadrillion,", " quadrilliard,", " quintillion,", " quintilliard,", " sextillion,")

  def numberToNumberAndString(StringOfANumber: String): String = {

    val stringOfALargeNumber = dotRemover(zeroRemover(StringOfANumber))
    val doubleOfALargeNumber = stringOfALargeNumber.toDouble
    val output = "Short scale:" + scaleName(stringOfALargeNumber, doubleOfALargeNumber, shortScaleList) +
      "\nLong scale:" + scaleName(stringOfALargeNumber, doubleOfALargeNumber, longScaleList)
    output
  }

  def scaleName(stringPart: String, doublePart: Double, format: List[String]): String = {
    val listNumberAsString = stringPart.reverse.grouped(3).toList.map(_.reverse.toList.mkString)
    val output = for (i <- listNumberAsString.indices) yield {
      i match {
        case 0 if listNumberAsString(i) != "000" => " and " + zeroRemover(listNumberAsString(i))
        case _ if listNumberAsString(i) == "000" => ""
        case _ if listNumberAsString(i) != "000" => " " + zeroRemover(listNumberAsString(i)) + format(i)
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
    Try {
      val output = numberToNumberAndString(input)
    } match {
      case Success(trying) =>
        val output = numberToNumberAndString(input)
        output
      case Failure(trying) => "Bad Input!"
    }
  }

}
