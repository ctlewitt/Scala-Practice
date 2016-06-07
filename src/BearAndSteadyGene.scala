import java.util.Scanner

/**
  * Created by charley on 6/6/16.
  * Finds the minimum length of a substring you can substitute out to make the given gene stable
  * (ie, all nucleotides appear an equal number of times)
  */
object BearAndSteadyGene {
  def main(args: Array[String]) {
    val sc = new Scanner(System.in)
    val n = sc.nextInt()
    val gene = sc.next()
    val lettersIHave: Map[Char, Int] = gene.groupBy(x=>x).mapValues(_.length)
    val lettersIDontWant = lettersIHave.mapValues(count => math.max(0, count - n/4))
    println(rubberBand(0, 0, lettersIDontWant, gene, gene.length))
  }

  //note start is inclusive; end is exclusive
  //lettersIDontWant is the count of the number of letters I need to find in a substring to replace
  //therefore, if I include a letter in the substring, I decrement the count of the number of letters I need to find to replace
  //and if I remove it from the substring (by contracting the rubberband) I need to increment the count because I might need to find another instance of that letter
  def rubberBand(start:Int, end:Int, lettersIDontWant: Map[Char,Int], gene: String, minSubStrLen: Int): Int ={
    //base case: your start has reached the end
   if(start == gene.length){
      minSubStrLen
   }
   else{
     if (lettersIDontWant.values.foldLeft(true){(accum , count) => if (count <= 0) accum else false }){
       rubberBand(start+1, end, lettersIDontWant + (gene.apply(start)->(lettersIDontWant(gene.apply(start)) + 1)), gene, math.min(end-start, minSubStrLen))
     }
     else{
       //another base case: if you don't have all of the letters you want in the current substring and your end marker is at the end of the gene
       if(end == gene.length){
         minSubStrLen
       }
       else {
         rubberBand(start, end + 1, lettersIDontWant + (gene.apply(end) -> (lettersIDontWant(gene.apply(end)) - 1)), gene, minSubStrLen)
       }
     }
   }
  }
}

