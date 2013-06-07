package com.scala.cspp51050
import scala.collection.mutable.Queue
import scala.collection.mutable.ListBuffer
class SearchAlgorithm {

}

object SearchUtils{
  def retrieveWordPairNode(word:Word,word2:Word):Option[WordPairNode] = {
     WordGraph.getNode(word.value.trim + "#" + word2.value.trim)
  }
 
  def tracePath(node:Option[BFSNode]) = node match{
    case Some(n) => {
      n match{
        case node: BFSGraphNode => {
                var list:ListBuffer[Word] = new ListBuffer[Word]
                tracePathAux(node,list, 1)
                list.foreach(f => print(f.value + " "))
               println
        }
        case _ => 
      }    
    }
    case None => //println("No max node returned")
  }
    
  def createLine(node:Option[BFSNode], lineNumber:Int):Option[Line] = node match{
     case Some(n) => {
      n match{
        case node: BFSGraphNode => {
                var list:ListBuffer[Word] = new ListBuffer[Word]
                var line:Line = new Line(lineNumber)
                tracePathAux(node,list,lineNumber)
                list.foreach(f => line.appendWord(f))
                return Some(line)
        }
        case _ => None
      }    
    }
    case None => None//println("No max node returned")
  }
      
  def getNode():WordPairNode = {
      val it = WordGraph.entries.values.iterator
      while(it.hasNext){
        var current = it.next
        if(current.forwardPointers.size>600){
          return current
        }
      }
      return WordGraph.entries.values.toList(2933)
  }

  
  private def tracePathAux(current:BFSNode, list:ListBuffer[Word], lineNum:Int):ListBuffer[Word] = current match{
    case c: BFSGraphNode => {
      list.prepend(c.value.value.first)
      tracePathAux(c.backPointer, list,lineNum)
    }
    case c: BFSRoot => {
      if(lineNum == 1)
      list.prepend(c.value.value.first)
      list
    }
  }
}
/*
class BreadthFirstSearch[X <: Component](val start:WordPairNode, val depth:Int) extends Algorithm[X]{
  var queue:Queue[BFSNode] = Queue.empty[BFSNode]
  var root:BFSRoot = new BFSRoot(start)
  var markedNodes: Set[String] = Set()
  def execute():X = {
    var done:Boolean = false
    queue.enqueue(root)
    while(!queue.isEmpty & done != true){
    	val node:BFSNode = queue.dequeue
    	if(node.distance>depth){
    	  done = true
    	}
    	node.value.forwardPointers.values.foreach(f =>
    	  SearchUtils.retrieveWordPairNode(node.value.value.second, f.value) match{
    	    case Some(w) => {
    	       if(!markedNodes.contains(w.key)){
    	       queue.enqueue(new BFSGraphNode(w, node,f.probability))
    	       markedNodes.+=(w.key)
    	       }
    	    }
    	    case None => "..."
    	  }
    	)	
      }
   }
  */
/*
    def getResult():Option[BFSNode] = {
      if (queue.size==0) None
  	  else  {
  	     chooseRandomResult(queue.toList.sorted)
  	     }
    }
    
    def chooseRandomResult(list:List[BFSNode]):Option[BFSNode] = {
            if(!list.isEmpty){
	    		val rand = GenRandInt(0,(list.size/2))
	    	    Some(list(rand.next))
    		}
            else None
    }
    
    def getName:String = "BreadthFirstSearch"
  
}*/

  abstract class BFSNode extends Ordered[BFSNode]{
    val value:WordPairNode
    def score:Double = 0
    def distance:Int = 0
    def compare(that:BFSNode)= {that.score compare this.score}
  }
  
 class BFSGraphNode(override val value:WordPairNode, 
    val backPointer:BFSNode, val transitionProb:Double) extends BFSNode{
    override def distance:Int = backPointer.distance + 1
    override def score:Double = {
       if(backPointer.score > 0)
    	   backPointer.score * this.transitionProb
         else
           this.transitionProb
    }
  }
  
  
 case class GenRandInt(lb :Int, ub:Int){
  private val rnd = new scala.util.Random
  def next() : Int = {
     if(lb == 0 && ub == 0) 0
     else lb + rnd.nextInt(ub)
  }
}
  
 class BFSRoot(override val value:WordPairNode) extends BFSNode
  
  