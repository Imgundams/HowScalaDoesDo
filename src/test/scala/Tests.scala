/*
Created by DRain on 18/07/2017.
*/
import org.scalatest._

class Tests extends FlatSpec with Matchers {
  val ScaleNumber = new NumberToScaleName

  //ZERO REMOVER FUNCTION
  "For a Number starting with zeros, the Zero return" should "remove any any prior zeros" in {
    val someZeros = "000000000000000000001001"
    ScaleNumber.zeroRemover(someZeros) shouldBe "1001"
  }

  //DOT REMOVER FUNCTION
  "For a Number with a decimal point, the dot function" should "return the number up to the point of the decimal" in {
    val dotInTheMiddle = "110010.0110101010"
    ScaleNumber.dotRemover(dotInTheMiddle) shouldBe "110010"
  }

  //SCALE NAME FUNCTION
  //Short Scale

  "with a correctly formatted number using short scale the scale name" should "output the correctly formatted string" in {
    val formattedNumber = "12345678901234567890"
    val formattedNumberDouble = formattedNumber.toDouble
    ScaleNumber.scaleName(formattedNumber, formattedNumberDouble, ScaleNumber.shortScaleList) shouldBe
      " 12 quintillion, 345 quadrillion, 678 trillion, 901 billion, 234 million, 567 thousand and 890"
  }

  //Long Scale

  "with a correctly formatted number using long scale the scale name" should "output the correctly formatted string" in {
    val formattedNumber = "12345678901234567890"
    val formattedNumberDouble = formattedNumber.toDouble
    ScaleNumber.scaleName(formattedNumber, formattedNumberDouble, ScaleNumber.longScaleList) shouldBe
      " 12 trillion, 345 billiard, 678 billion, 901 milliard, 234 million, 567 thousand and 890"
  }

  //NUMBER TO NUMBER AND STRING FUNCTION
  "The number to a number and string function" should "return both the short and long scale" in {
    val input = "12345678901234567890"
    ScaleNumber.numberToNumberAndString(input) shouldBe
      "Short scale: 12 quintillion, 345 quadrillion, 678 trillion, 901 billion, 234 million, 567 thousand and 890" +
        "\nLong scale: 12 trillion, 345 billiard, 678 billion, 901 milliard, 234 million, 567 thousand and 890"
  }

  //INPUT ENTRY
  "The standard Input entry function" should "return done when worked" in {
    val input = "12345678901234567890"
    ScaleNumber.inputEntry(input) shouldBe
      "Short scale: 12 quintillion, 345 quadrillion, 678 trillion, 901 billion, 234 million, 567 thousand and 890" +
        "\nLong scale: 12 trillion, 345 billiard, 678 billion, 901 milliard, 234 million, 567 thousand and 890"
  }
  "A bad Input entry function" should "return Bad Input!" in {
    val input = "something gone wrong!"
    ScaleNumber.inputEntry(input) shouldBe
      "Bad Input!"
  }

}