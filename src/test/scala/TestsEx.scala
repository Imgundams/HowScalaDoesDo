
//Created by DRain on 18/07/2017.

import org.scalatest._
import Main.{zeroRemover, scaleName, dotRemover, numberToNumberAndString, inputEntry}

class TestsEx extends FlatSpec with Matchers {

  val shortScaleList = List("", " thousand", " million,", " billion,", " trillion,", " quadrillion,",
    " quintillion,", " sextillion,", " septillion,", " octillio,n", " nonillion,", " decillion,", " undecillion,")

  val longScaleList = List(" ", " thousand", " million,", " milliard,", " billion,", " billiard,",
    " trillion,", " trilliard,", " quadrillion,", " quadrilliard,", " quintillion,", " quintilliard,", " sextillion,")

  //ZERO REMOVER FUNCTION
  "With a formatted number the Zero return" should "not return any prior zeros" in {
    val noZeros = "1001"
    zeroRemover(noZeros) shouldBe noZeros
  }
  "With a formatted number, with tailing zeros the Zero return" should "not return any prior zeros" in {
    val tailingZeros = "100100"
    zeroRemover(tailingZeros) shouldBe tailingZeros
  }
  "With a non-formatted number the Zero return" should "remove any any prior zeros" in {
    val someZeros = "001001"
    zeroRemover(someZeros) shouldBe "1001"
  }

  //DOT REMOVER FUNCTION
  "With a formatted number and no dots with the dot function" should "return itself" in {
    val noDotsHere = "1001"
    dotRemover(noDotsHere) shouldBe noDotsHere
  }
  "With a formatted number the with the dot function to remove the dot " should "return itself without the dot" in {
    val dotInTheMiddle = "10.01"
    dotRemover(dotInTheMiddle) shouldBe "10"
  }
  "With a formatted number the with the dot function" should "only return numbers before the dot" in {
    val manyDotsHere = "101.01.010.1..0.0.1"
    dotRemover(manyDotsHere) shouldBe "101"
  }

  //SCALE NAME FUNCTION
  //Short Scale

  "with a formatted number using short scale the scale name" should "output the correctly formatted string" in {
    val formattedNumber = "12345678910"
    val formattedNumberDouble = formattedNumber.toDouble
    scaleName(formattedNumber, formattedNumberDouble, shortScaleList) shouldBe
      " 12 billion, 345 million, 678 thousand and 910"
  }
  "with a formatted number with tailing zeros and 1 using short scale the scale name" should
    "output the correctly formatted string" in {
    val formattedNumber = "12345678910000001"
    val formattedNumberDouble = formattedNumber.toDouble
    scaleName(formattedNumber, formattedNumberDouble, shortScaleList) shouldBe
      " 12 quadrillion, 345 trillion, 678 billion, 910 million, and 1"
  }
  "with a formatted number with tailing zeros using short scale the scale name" should
    "output the correctly formatted string" in {
    val formattedNumber = "12345678910000000"
    val formattedNumberDouble = formattedNumber.toDouble
    scaleName(formattedNumber, formattedNumberDouble, shortScaleList) shouldBe
      " 12 quadrillion, 345 trillion, 678 billion, 910 million,"
  }
  //Long Scale

  "with a formatted number using long scale the scale name" should "output the correctly formatted string" in {
    val formattedNumber = "12345678910"
    val formattedNumberDouble = formattedNumber.toDouble
    scaleName(formattedNumber, formattedNumberDouble, longScaleList) shouldBe
      " 12 milliard, 345 million, 678 thousand and 910"
  }
  "with a formatted number with tailing zeros and 1 using long scale the scale name" should
    "output the correctly formatted string" in {
    val formattedNumber = "12345678910000001"
    val formattedNumberDouble = formattedNumber.toDouble
    scaleName(formattedNumber, formattedNumberDouble, longScaleList) shouldBe
      " 12 billiard, 345 billion, 678 milliard, 910 million, and 1"
  }
  "with a formatted number with tailing zeros using long scale the scale name" should
    "output the correctly formatted string" in {
    val formattedNumber = "12345678910000000"
    val formattedNumberDouble = formattedNumber.toDouble
    scaleName(formattedNumber, formattedNumberDouble, longScaleList) shouldBe
      " 12 billiard, 345 billion, 678 milliard, 910 million,"
  }

  //NUMBER TO NUMBER AND STRING FUNCTION
  "The number to a number and string function" should "return both the short and long scale" in {
    val input = "123456789012345"
    numberToNumberAndString(input) shouldBe
      "Short scale: 123 trillion, 456 billion, 789 million, 12 thousand and 345" +
        "\nLong scale: 123 billion, 456 milliard, 789 million, 12 thousand and 345"
  }

  //INPUT ENTRY
  "The Input entry function" should "return done when worked" in {
    val input = "123456789012345"
    inputEntry(input) shouldBe
      "Short scale: 123 trillion, 456 billion, 789 million, 12 thousand and 345\n" +
        "Long scale: 123 billion, 456 milliard, 789 million, 12 thousand and 345"
  }
  "A bad Input entry function" should "return Bad Input!" in {
    val input = "something gone wrong!"
    inputEntry(input) shouldBe
      "Bad Input!"
  }
  "A Number too large inputted in the entry function" should "return Bad Input!" in {
    val input = "1234567890123456789012345678901234567890"
    inputEntry(input) shouldBe
      "Bad Input!"
  }
  "The largest number that can be inputted in the entry function" should "return Bad Input!" in {
    val input = "123456789012345678901234567890123456789"
    inputEntry(input) shouldBe
      "Short scale: 123 undecillion, 456 decillion, 789 nonillion, 12 octillio,n 345 septillion, 678 sextillion," +
        " 901 quintillion, 234 quadrillion, 567 trillion, 890 billion, 123 million, 456 thousand and 789" +
        "\nLong scale: 123 sextillion, 456 quintilliard, 789 quintillion, 12 quadrilliard, 345 quadrillion," +
        " 678 trilliard, 901 trillion, 234 billiard, 567 billion, 890 milliard, 123 million, 456 thousand and 789"
  }

}