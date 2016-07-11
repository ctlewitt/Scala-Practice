import scala.io.StdIn

/**
  * Created by charley and Jeremie on 7/7/16.
  * https://www.hackerrank.com/challenges/connected-cell-in-a-grid
  */


class Matrix(grid: Seq[Seq[Int]]){
  val (numRows, numCols) = (grid.size, grid.head.size)


  def getMaxConn(x: Int, y: Int) : (Int, Matrix) = {
    //call next recursion
    val (currSize, updatedMatrix) = countComponent(x, y) //CONSIDERING ADDING OURSELVES INTO LIST OF NEIGHBORS HERE
    //    val (currSize, updatedMatrix) = countComponent(getNeighbors(x,y, numRows, numCols), matrix) //CONSIDERING ADDING OURSELVES INTO LIST OF NEIGHBORS HERE
    getNextPos(x, y) match{
      case Some((i,j)) =>
        val (maxSize, newUpdatedMatrix) = updatedMatrix.getMaxConn(i, j)
        (Math.max(currSize, maxSize), newUpdatedMatrix)
      case None =>
        (currSize, updatedMatrix)
    }
  }

  def getNextPos(x:Int, y:Int): Option[(Int, Int)] = {
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

  def countComponent(x:Int, y:Int):  (Int, Matrix) = {
    if (grid(y)(x) == 0) {
      (0, this)
    }
    else {
      val matrixWZero = setToZero(x, y)
      val (count, updatedMatrix) = matrixWZero.countComponentAux(getNeighbors(x, y))
      (count + 1, updatedMatrix)
    }
  }


  def countComponentAux(neighbors: Seq[(Int, Int)]): (Int, Matrix) ={
    neighbors match {
      case (x,y)::tail =>
        if (grid(y)(x) == 0) {
          countComponentAux(tail)
        }
        else{
          val myNeighbors = getNeighbors(x,y)
          val (newNeighborCount, newNeighborMatrix) = setToZero(x,y).countComponentAux(myNeighbors)
          val (myUpdatedCount, myUpdatedMatrix) = newNeighborMatrix.countComponentAux(tail)
          (1 + myUpdatedCount + newNeighborCount, myUpdatedMatrix)
        }
      case Nil =>
        (0, this)
    }
  }


  def getNeighbors(x: Int, y: Int): Seq[(Int, Int)] = {
    val  l = List(-1, 0, 1)
    val pairs = for {
      i <- l
      j <- l
      if i != 0 || j!= 0
    } yield (i, j)
    pairs.map({case (i, j) => (i+x, j+y)}).filter({case (i,j) => i>=0 && i<numCols && j>=0 && j<numRows})
  }

  def setToZero(x:Int, y:Int) : Matrix = {
    new Matrix(grid.updated(y, grid(y).updated(x, 0)))
  }



}



object ConnectedCellInGrid {
  type Value = Int

  def main(args: Array[String]) {
    val numRows = StdIn.readInt()
    val numCols = StdIn.readInt()
    val grid: Seq[Seq[Value]] = {
      (0 until numRows).map(_ => StdIn.readLine().split(" ").toSeq.map(_.toInt))
    }
    val matrix: Matrix = new Matrix(grid)

    val (maxConCompCount, _) = matrix.getMaxConn(0, 0)
    println(maxConCompCount)
    //if not already checked and 1, search connected component, keep track of size
    //return max size
  }
}



