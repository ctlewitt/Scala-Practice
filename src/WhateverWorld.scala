/**
  * Created by charley on 5/26/16.
  */


//singleton or static class
case object EmptyNode extends Node{
  val hasChildren: Boolean = false
}

case class FullNode(left: Node, right: Node, value: Integer) extends Node{
  val hasChildren: Boolean = (left!=EmptyNode || right!=EmptyNode)
  var myTest = 13
  def setLeft(left: Node): Unit ={

  }
}


//case classes don't need new
//case classes don't need val/var in the parameters; they're automatically saved going forward;  and they enable pattern matching
//don't need to use "new" with a case class

abstract class Node{
  val hasChildren: Boolean

  def changeLeftMostChild(value:Int) : Node = {
    this match {
      case FullNode(EmptyNode, right, _ ) =>
          FullNode(EmptyNode, right, value)
      case FullNode(left, right, curVal) =>
          FullNode(left.changeLeftMostChild(value), right, curVal)
      case EmptyNode =>
        Node.leaf(value)
    }
  }

}


//companion objects contain methods that behave like static methods for a class (not associated with an instance; just the class)
object Node{
  def leaf(value:Integer): FullNode = {
    FullNode(EmptyNode, EmptyNode, value)
  }

  def computeSomething(): Int = {
    var (i,j,k) =(1,2,3)
    i = 54
    k
  }


}


object WhateverWorld extends App{
  val puppy = new Puppy()
  val leaf = Node.leaf(3) //get used to it; everything is new
  val leftSubtree = FullNode(new FullNode(leaf, EmptyNode, 2), leaf, 52)
  val root = FullNode(leftSubtree, EmptyNode, 0)
  println(root)
  val newTree = root.changeLeftMostChild(5)
  println(newTree)
}

class Puppy extends App {
  val name: String = "ginger"
  println(name)
}


//scala: everything is an object/class (like Ruby)