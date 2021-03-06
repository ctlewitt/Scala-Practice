import java.util.Scanner

import scala.collection.immutable.{HashMap, List}
import scala.math


/*These are some problems (mainly about arrays) from Hackerrank*/
object Solution {

  //REVERSE ARRAY
  def reverseArray(args: Array[String]) {
    val sc = new Scanner(System.in)
    var n = sc.nextInt()
    var arr = new Array[Int](n)
    for(arr_i <- 1 to n) {
      arr(n - arr_i) = sc.nextInt()
    }


    for(arr_i <- 0 until n) {
      print(arr(arr_i))
      if (arr_i < n-1){
        print(" ")
      }
    }
  }



  //2D ARRAY SEARCH (HOURGLASS)
  def hourGlass(args: Array[String]) {
    val sc = new Scanner (System.in)
    var arr = Array.ofDim[Int](6,6)
    for(arr_i <- 0 until 6) {
      for(arr_j <- 0 until 6){
        arr(arr_i)(arr_j) = sc.nextInt()
      }
    }
    var myMax = Integer.MIN_VALUE
    for(arr_y <- 0 to 3){
      for(arr_x <- 0 to 3){
        val nextHourGlass = hourglass(arr, arr_x, arr_y)
        if (nextHourGlass > myMax) {
          myMax = nextHourGlass
        }
      }
    }
    print (myMax)
  }

  def hourglass(arr:Array[Array[Int]], x_idx: Int, y_idx: Int):Int = {
    val rowOneSum:Int = rowSum(arr, x_idx, y_idx, 2)
    val rowTwoSum:Int = rowSum(arr, x_idx, y_idx+2, 2)
    val middle:Int = arr(y_idx + 1)(x_idx + 1)
    val sum = rowOneSum + rowTwoSum + middle
    sum
  }

  def rowSum(arr:Array[Array[Int]], x_idx: Int, y_idx: Int, count:Int) : Int = {
    if(count > 0) {
      arr(y_idx)(x_idx+count) + rowSum(arr, x_idx, y_idx, count -1)
    }
    else{
      arr(y_idx)(x_idx)
    }
  }

  //dynamic array
  def dynamicArray(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val q = sc.nextInt()
    //create 2D array of ints
    val myArr = Array.fill(n){new Array[Int](0)}
    //last ans will be used later
    var lastAns = 0
    for( currQuery <- Range(0, q)){
      val queryType = sc.nextInt()
      val x = sc.nextInt()
      val y = sc.nextInt()

      val seqIdx = (x^lastAns) % n
      if(queryType == 1){
        myArr(seqIdx) = myArr(seqIdx):+y //DOES THIS RETURN A VALUE OR UPDATE THE ACTUAL???
      }
      else{//queryType ==2
        lastAns = myArr(seqIdx)(y % myArr(seqIdx).length)
        println(lastAns)
      }
    }
  }




  //sparse array
  def sparseArray(args: Array[String]) {
    /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution
*/
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    var myWords = sparseArrAux(new HashMap[String, Int](), n, sc)
    val q = sc.nextInt()
    for(a <- Range(0, q)){
      val nextWordQuery = sc.next()
      println (myWords getOrElse (nextWordQuery,  0) )
    }
    sc.close()

  }

  //helper for sparse array
  def sparseArrAux(oldHash: HashMap[String, Int], count: Int, sc: Scanner): HashMap[String, Int] ={
    if(count > 0){
      val nextWord: String = sc.next()
      val nextWordCount = oldHash.getOrElse(nextWord, -1)
      if(nextWordCount == -1) {
        sparseArrAux(oldHash.+(nextWord -> 1), count - 1, sc)
      }
      else{
        sparseArrAux(oldHash.updated(nextWord, nextWordCount + 1), count-1, sc)
      }
    }
    else{
      oldHash
    }
  }



  def maxElement() {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    maxElementAux(List(), n, sc)
  }

  def maxElementAux(myStack: List[StackElem], count: Int, sc: Scanner): Unit ={
    if(count > 0){
      val command: Int = sc.nextInt()
      if (command == 1){
        val numToAdd = sc.nextInt()
        if(myStack.isEmpty){
          maxElementAux(new StackElem(numToAdd, numToAdd)+:myStack, count-1, sc)
        }
        else{
          maxElementAux(new StackElem(numToAdd, math.max(numToAdd, myStack.head.curMax))+:myStack, count-1, sc)
        }
      }
      else if (command == 2){
        maxElementAux(myStack.tail, count-1, sc)
      }
      else{//command == 3
        println(myStack.head.curMax)
        maxElementAux(myStack, count-1, sc)
      }
    }

  }

  class StackElem(val value:Int, val curMax:Int){}

  //MaximizingXOR
  def MaximizingXOR() {
    val sc = new Scanner(System.in)
    val leftMost = sc.nextInt()
    val rightMost = sc.nextInt()
    sc.close()
    println (maxXORVaryL(leftMost, rightMost, 0))
  }

  //helper for maximizingXOR
  def maxXORVaryL(left: Int, rightMost: Int, maxXOR: Int): Int={
    if (left > rightMost){
      maxXOR
    }
    else{
      var maxXORUpdated = maxXORVaryR(left, left, rightMost, maxXOR)
      maxXORVaryL(left+1, rightMost, maxXORUpdated)
    }
  }

  //helper for maximizingXOR
  def maxXORVaryR(left: Int, right: Int, rightMost: Int, maxXOR: Int): Int= {
    if(right > rightMost){
      maxXOR
    }
    else{
      math.max(math.max(maxXOR, left^right), maxXORVaryR(left, right+1, rightMost, maxXOR))
    }
  }


}
