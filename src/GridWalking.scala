/**
  * Created by charley on 6/7/16.
  */
object GridWalking {

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    val numTests = sc.nextInt()
    //go through each test
    for(_ <- 0 until numTests){
      val numDims = sc.nextInt()
      val numSteps = sc.nextInt()
      //get starting position
      val start_pos = fillInList(List(), numDims, sc)
      //get dimension of each row
      val dimOfRow = fillInList(List(), numDims, sc)

    }



  }

  //helper function: scans in values to populate a list
  def fillInList(currList: List[Int], numElemsLeft: Int, sc:java.util.Scanner): List[Int] ={
    if(numElemsLeft > 0){
      fillInList(currList:+sc.nextInt(), numElemsLeft-1, sc)
    }
    else{
      currList
    }
  }


}
