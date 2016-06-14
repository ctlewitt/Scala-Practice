/**
  * Created by charley and michael on 6/14/16.
  */
object GridSearch {
  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    val numTests = sc.nextInt()
    for (_ <- 1 to numTests) {
      val numBigRows = sc.nextInt()
      val numBigCols = sc.nextInt()
      var bigGrid = ""
      for (gridIdx <- 0 until numBigRows) {
        bigGrid += sc.next()
      }
      val numSmallRows = sc.nextInt()
      val numSmallCols = sc.nextInt()
      var smallGrid = sc.next()
      val rowLenDiff = numBigCols - numSmallCols

      for (smallGridIdx <- 1 until numSmallRows) {
        smallGrid += ("." * rowLenDiff ++ sc.next())
      }
      if (smallGrid.r().findFirstIn(bigGrid).isDefined) {
        println("YES")
      }
      else {
        println("NO")
      }
    }
  }
}
