/**
  * Created by charley on 6/7/16.
  *
  *
  *
  * NEXT STEP: MAKE FASTER BY DETERMINING EACH MOVE FROM THE PREVIOUS MOVES INSTEAD OF GOING THROUGH EVERY MOVE EVERY TIME
  * POSSIBLE BUG: FOR A POSITION (E.G., 22322) I AM CURRENTLY ONLY ADDING 22222 AND 22422 TO REACH THAT NUMBER, BUT I SHOULD
  * ALSO BE ADDING 12322 AND 32322 AND ETC...2 FOR EACH DIMENSION. 
  *
  *
  */
object GridWalking {

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    val numTests = sc.nextInt()
    //go through each test
    for(_ <- 0 until numTests){
      val numDims = sc.nextInt()
      val numMoves = sc.nextInt()
      //get starting position
      val start_pos = fillInList(List(), numDims, sc)
      //get dimension of each row
      val dimOfRow = fillInList(List(), numDims, sc)
      val allPositions: List[List[Int]] = generateAllPositions(dimOfRow)
      val initialMap: Map[List[Int], BigInt] = Map() + (start_pos -> 1)
      val finalMap = makeAllMoves(initialMap, numMoves, allPositions)
      val totalPossibleMoves = finalMap.values.sum
      println(totalPossibleMoves)
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

  def makeAllMoves(possibleMovesToDate: Map[List[Int], BigInt], numMovesLeft: Int, allPositions: List[List[Int]]) : Map[List[Int], BigInt] = {
    if(numMovesLeft > 0){
      makeAllMoves(makeOneMove(possibleMovesToDate, allPositions), numMovesLeft-1, allPositions)
    }
    else{
      possibleMovesToDate

    }

  }

  def makeOneMove(possibleMovesToDate: Map[List[Int], BigInt], allPositions: List[List[Int]]): Map[List[Int], BigInt] ={
    var updatedPossibleMoves: Map[List[Int], BigInt] = Map()
    for (pos <- allPositions){
      updatedPossibleMoves = updatedPossibleMoves.updated(pos, makeMiniMove(pos,possibleMovesToDate)) //MIGHT NEED TO SAY UPDATEDPOSSIBLEMOVES = THIS
    }
    updatedPossibleMoves
  }


  //helper function: gets total number of moves options that could lead to this position
  def makeMiniMove(position: List[Int], possibleMovesToDate: Map[List[Int], BigInt]): BigInt ={
    var sum:BigInt = 0
    for (idx <- position.indices){ //try "position.indicies" instead
      val positionPlus = position.updated(idx, position.apply(idx) +1)
      val positionMinus = position.updated(idx, position.apply(idx) -1)
      sum += possibleMovesToDate.getOrElse(positionPlus, 0)
      sum += possibleMovesToDate.getOrElse(positionMinus, 0)
    }
    sum
  }

  //helper function: generates a list containing lists that represent the possible positions in the dimOfRow.length-dimensional board
  def generateAllPositions(dimOfRow: List[Int]) : List[List[Int]] ={
    if(dimOfRow.nonEmpty){
      var updatedListOfPositions: List[List[Int]] = List()

      for(pos <- 1 to dimOfRow.head) {
        updatedListOfPositions = updatedListOfPositions :+ List(pos)
      }
      generateAllPositionsAux(dimOfRow.tail, updatedListOfPositions)
    }
    else{
      List()
    }
  }

  //helper function: generates a list containing lists that represent the possible positions in the dimOfRow.length-dimensional board
  def generateAllPositionsAux(dimOfRow: List[Int], listOfPositions: List[List[Int]]) : List[List[Int]] ={
    if(dimOfRow.nonEmpty){
      var updatedListOfPositions: List[List[Int]] = List()

      for(pos <- 1 to dimOfRow.head) {
        updatedListOfPositions = updatedListOfPositions ::: listOfPositions.mapConserve[List[Int]](myList => myList :+ pos)
      }
      generateAllPositionsAux(dimOfRow.tail, updatedListOfPositions)
    }
    else{
      listOfPositions
    }
  }
}
