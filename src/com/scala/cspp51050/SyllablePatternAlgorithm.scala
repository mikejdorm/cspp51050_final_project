package com.scala.cspp51050
import scala.collection.mutable.Queue
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Set

class SyllablePatternAlgorithm(val start:WordPairNode, val syllableCount:Int) extends Algorithm{
  var queue:Queue[BFSyllableNode] = Queue.empty[BFSyllableNode]
  var root:BFSSyllableRoot = new BFSSyllableRoot(start, (start.value.first.syllableCount +
		  						start.value.second.syllableCount))
  var results:ListBuffer[BFSyllableNode] = new ListBuffer[BFSyllableNode]
  var markedNodes: Set[String] = Set()
  
  def execute() = {
    queue.enqueue(root)
    while(!queue.isEmpty){
    	val node:BFSyllableNode = queue.dequeue
    	node.value.forwardPointers.values.foreach(f =>
    	  SearchUtils.retrieveWordPairNode(node.value.value.second, f.value) match{
    	    case Some(w) => {
    	      processNode(node,w,f)
    	    }
    	    case None => 
    	  }
    	)	
      }
   }
  
  def processNode(parent:BFSyllableNode, child:WordPairNode, newWord:WordNode) = {
         val newCount:Int = Int.box(parent.syllableCount+newWord.value.syllableCount)
         val target:Int = this.syllableCount
         if(newCount.compareTo(target)==0){
    	    results.append(new BFSSyllableGraphNode(child,parent,newCount, newWord.probability))
    	 }
    	 else if(newCount.compareTo(target)<0){
    	    if(!markedNodes.contains(child.key)){
	    	    queue.enqueue(new BFSSyllableGraphNode(child,parent,newCount, newWord.probability))
	    	   // println(child.key + " not in set " + markedNodes.size)
	    	    markedNodes.+=(child.key)
	    	    //    println("added. size now: " + markedNodes.size)
    	    }
    	  }
  }
  
    def getResult():Option[BFSNode] = {
      if (results.isEmpty) None
      else if(results.size == 1) return Some(results.toList(0))
  	  else  Some(chooseRandomResult(results.toList))
    }
    
    def chooseRandomResult(list:List[BFSNode]):BFSNode = {  
        list.foreach(res => SearchUtils.createLine(Some(res),1)
            match {
            	case Some(l) => l.printLine
          		case None => "none"
        	})
        val rand = GenRandInt(0,(list.size/2))
		list(rand.next)
    }
    
    def getName:String = "BreadthFirstSearch"
}

trait BFSyllableNode extends BFSNode{
  val syllableCount:Int
  def compare(that:BFSyllableNode)= {that.score compare this.score}
}

class BFSSyllableRoot(override val value:WordPairNode, val syllableCount:Int) extends BFSRoot(value) 
with BFSyllableNode

class BFSSyllableGraphNode(override val value:WordPairNode, override val backPointer:BFSNode,  val syllableCount:Int, 
   override val transitionProb:Double) extends BFSGraphNode(value,backPointer,transitionProb) with BFSyllableNode
  