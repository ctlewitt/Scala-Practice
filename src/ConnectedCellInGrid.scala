import scala.io.StdIn

/**
  * Created by charley and Jeremie on 7/7/16.
  * https://www.hackerrank.com/challenges/connected-cell-in-a-grid
  */


object ConnectedCellInGrid {
  type Value = Int

  def main(args: Array[String]) {
    val numRows = StdIn.readInt()
    val numCols = StdIn.readInt()
    val matrix: Seq[Seq[Value]] = {
      (0 until numRows).map(_ => StdIn.readLine().split(" ").toSeq.map(_.toInt))
    }

    val (maxConCompCount, finishedMatrix) = getMaxConn(matrix, 0, 0)
    println(maxConCompCount)
    //if not already checked and 1, search connected component, keep track of size
    //return max size
  }


  def getMaxConn(matrix: Seq[Seq[Value]], x: Int, y: Int) : (Int, Seq[Seq[Value]]) = {
    //call next recursion
    val (numRows, numCols) = getDims(matrix)
    val (currSize, updatedMatrix) = countComponent(matrix, x, y) //CONSIDERING ADDING OURSELVES INTO LIST OF NEIGHBORS HERE
//    val (currSize, updatedMatrix) = countComponent(getNeighbors(x,y, numRows, numCols), matrix) //CONSIDERING ADDING OURSELVES INTO LIST OF NEIGHBORS HERE
    getNextPos(x, y, matrix) match{
      case Some((i,j)) =>
        val (maxSize, newUpdatedMatrix) = getMaxConn(updatedMatrix, i, j)
        (Math.max(currSize, maxSize), newUpdatedMatrix)
      case None =>
        (currSize, updatedMatrix)
    }
  }

  def getNextPos(x:Int, y:Int, matrix: Seq[Seq[Value]]): Option[(Int, Int)] = {
    val (numRows, numCols) = getDims(matrix)
    if(x < numCols-1){
      Some(x+1, y)
    }
    else if(y < numRows-1){
      Some(0,y+1)
    }
    else{
      None
    }
  }

//!!!!!!!!!!!!!!!!this was commented out before
  def countComponent(matrix: Seq[Seq[Value]],x:Int, y:Int):  (Int, Seq[Seq[Value]]) = {
  if (matrix(y)(x) == 0) {
    (0, matrix)
  }
  else {
    val matrixWZero = setToZero(matrix, x, y)
    val (numRows, numCols) = getDims(matrix)
    val (count, updatedMatrix) = countComponentAux(getNeighbors(x, y, numRows, numCols), matrixWZero)
    (count + 1, updatedMatrix)
  }
}


  def countComponentAux(neighbors: Seq[(Int, Int)], matrix: Seq[Seq[Value]]): (Int, Seq[Seq[Value]]) ={
    neighbors match {
      case (x,y)::tail =>
        if (matrix(y)(x) == 0) {
          countComponentAux(tail, matrix)
        }
        else{
          val (numRows, numCols) = getDims(matrix)
          val myNeighbors = getNeighbors(x,y, numRows, numCols)
          val (newNeighborCount, newNeighborMatrix) = countComponentAux(myNeighbors, setToZero(matrix, x,y))
          val (myUpdatedCount, myUpdatedMatrix) = countComponentAux(tail, newNeighborMatrix)
          (1 + myUpdatedCount + newNeighborCount, myUpdatedMatrix)
        }
      case Nil =>
        (0, matrix)
    }
  }

  def getDims(matrix: Seq[Seq[Int]]): (Int, Int) ={
    (matrix.size, matrix.head.size)
  }

  def getNeighbors(x: Int, y: Int, numRows: Int, numCols: Int): Seq[(Int, Int)] = {
    val  l = List(-1, 0, 1)
    val pairs = for {
      i <- l
      j <- l
      if i != 0 || j!= 0
    } yield (i, j)
    pairs.map({case (i, j) => (i+x, j+y)}).filter({case (i,j) => i>=0 && i<numCols && j>=0 && j<numRows})
  }

  def setToZero(matrix: Seq[Seq[Int]], x:Int, y:Int) : Seq[Seq[Int]] = {
    matrix.updated(y, matrix(y).updated(x, 0))
  }

}



