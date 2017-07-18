import org.scalatest._

//Created by DRain on 18/07/2017.
class Tests extends FlatSpec {
	//ZERO REMOVER FUNCTION
	"With a formatted number the Zero return" should "not return any prior zeros" in {
		val noZeros = "1001"
		assert(Main.zeroRemover(noZeros) === noZeros)
	}
	"With a formatted number, with tailing zeros the Zero return" should "not return any prior zeros" in {
		val tailingZeros = "100100"
		assert(Main.zeroRemover(tailingZeros) === tailingZeros)
	}
	"With a non-formatted number the Zero return" should "remove any any prior zeros" in {
		val someZeros = "001001"
		assert(Main.zeroRemover(someZeros) === "1001")
	}

	//DOT REMOVER FUNCTION
	"With a formatted number and no dots with the dot function" should "return itself" in {
		val noDotsHere = "1001"
		assert(Main.dotRemover(noDotsHere) === noDotsHere)
	}
	"With a formatted number the with the dot function to remove the dot " should "return itself without the dot" in {
		val dotInTheMiddle = "10.01"
		assert(Main.dotRemover(dotInTheMiddle) === "10")
	}
	"With a formatted number the with the dot function" should "only return numbers before the dot" in {
		val manyDotsHere = "101.01.010.1..0.0.1"
		assert(Main.dotRemover(manyDotsHere) === "101")
	}
}