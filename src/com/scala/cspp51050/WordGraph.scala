package com.scala.cspp51050
import scala.collection.mutable.HashMap

class Graph{
  
}
object WordGraph extends Graph{
  var entries:HashMap[String,WordPairNode] = new HashMap[String, WordPairNode]
  def addEntry(node:WordPairNode) = entries.put(node.key, node)
  def size:Int = entries.size
  def addForwardPointerToNode(key:String, word:WordNode) = {
    println("Adding forward pointer: "  + word.key +  " to " + key)
     entries.get(key) match{
       case Some(x) => x.addForwardPointer(word)
       case None => println("interesting")
     }
  }
  
  def addBackPointerToNode(key:String, word:WordNode) = {
       println("Adding back pointer: "  + word.key +  " to " + key)
    entries.get(key) match{
      case Some(x) => x.addBackPointer(word)
      case None => println("ok")
    }
  }
  
  def addForwardPointerToNode(key:String, word:Word) = {
    
  }
  
  def addBackPointerToNode(key:String, word:Word) ={
    entries.get(key) match{
      case Some(x) => x.addBackPointer(new WordNode(word,x))
      case None => println("")
    }
  }
  def getNode(word1:Option[Word], word2:Option[Word]) = word1 match{
    case Some(x) => word2 match{
      case Some(y) => entries.get(x.value + "#" + y.value)
      case None => None
    }
    case None => None
  }
  
  def getNode(key:String):Option[WordPairNode] = entries.get(key)
}