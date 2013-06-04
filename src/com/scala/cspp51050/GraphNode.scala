package com.scala.cspp51050
import scala.collection.mutable.HashMap

abstract class GraphNode[X] {
  def key:String
  val value:X
}

case class WordPairNode(val value:WordPair) extends GraphNode[WordPair]{
  var counter:Int = 0
  override def key:String = value.first.value+"#"+value.second.value
 
  var forwardPointers:HashMap[String,WordNode] = new HashMap[String,WordNode]
  var backPointers:HashMap[String,WordNode] = new HashMap[String,WordNode]

  def incrementMasterCounter() = {this.counter = this.counter + 1}
  
  private def addPointer(map:HashMap[String, WordNode], x:WordNode) = {
    if(map.contains(x.value.value)){
      map.get(x.value.value) match{
        case Some(x) => {
          this.incrementMasterCounter()
          x.incrementCount()
        }
        case None => println("What!?")
      }
    }
    else{
        this.incrementMasterCounter()
          x.incrementCount()
      map.put(x.key,x)
    }
  }
  
  def addForwardPointer(x:WordNode) = {
    addPointer(forwardPointers,x)
  }
  def addBackPointer(x:WordNode) = {
    addPointer(backPointers,x)
  }
  
  def getRandomForwardTransition():WordNode = forwardPointers.values.last //TODO add in a random funl
}
  class WordPair(val first:Word, val second:Word)
  
  case class WordNode(val value:Word, val parent:WordPairNode) extends GraphNode[Word]{
     var counter:Int = 0
     override def key:String = value.value
     def incrementCount() = {this.counter = this.counter +1 }
     def probability:Double = {
       if(parent.counter != 0.0){
         value.setProbability(this.counter/parent.counter)
       //  println(value.probability)
       }
        value.probability
     } 
     
  }
  