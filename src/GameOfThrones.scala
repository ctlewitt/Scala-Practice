import java.util.Scanner
import scala.math.{BigInt, pow}
import scala.util.Random


object GameOfThrones {

  def main(args: Array[String]) {
    palindromeChecker()
    getNumPalindromes()

  }

  def palindromeChecker() :Unit = {
    val sc = new Scanner(System.in)
    val myStr = sc.next()
    val grouped = myStr.groupBy(x=>x)
    val numOdds = grouped.values.count(str => str.length % 2 != 0)
    if (numOdds > 1){
      println("NO")
    }
    else{
      println("YES")
    }
  }

  //getNumPalindromes
  def getNumPalindromes(): Unit = {
    val sc = new java.util.Scanner(System.in)
    val myMod: BigInt = BigInt(pow(10,9).toLong + 7)
    try {
      val myStr = sc.next()
      //For testing, can use stringMaker to generate a string of any length that contains at least 1 palindrome
      //val myStr:String = stringMaker(pow(10, 5).toLong, "")
      val grouped = myStr.groupBy(x => x)
      val halfCharCountList: Iterable[BigInt] = grouped.values.map(str => BigInt(str.length / 2))
      val totalLetterCount: BigInt = halfCharCountList.sum //fold(0) { (accum, count) => count + accum }
      val factorialProdOfEachCounts: BigInt = halfCharCountList.fold(BigInt(1)) { (accum, count) => fact(count, 1) * accum }
      val factorialOfTotalLetters = fact(totalLetterCount, 1)
      println((factorialOfTotalLetters / factorialProdOfEachCounts) % myMod)
      sc.close()
    }
  }

  //helper function: calculates factorial of num
  def fact(num:BigInt, factToDate:BigInt): BigInt ={
    val myMod = (pow(10,9) + 7).toLong
    if(num>1) fact(num-1, factToDate * num)
    else factToDate
  }

  //helper function for debugging: generates a string of length len that contains at least 1 palindrome for testing
  def stringMaker(len:Long, myStr:String): String = {
    if (len > 0){
      val nextChar:Char =  (Random.nextInt(26)+97).toChar
      val tempStr: String = nextChar.toString
      if(len > 1) {
        stringMaker(len-2, myStr.concat(tempStr).concat(tempStr))
      }
      else{
        stringMaker(len-1, myStr.concat(tempStr))
      }
    }
    else {
      myStr
    }
  }
}
