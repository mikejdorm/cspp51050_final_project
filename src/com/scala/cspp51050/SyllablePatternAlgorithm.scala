package com.scala.cspp51050
import scala.collection.mutable.Queue
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Set

class SyllablePatternAlgorithm(val start:WordPairNode, 
  val syllableCount:Int, val lineNumber:Int, val reverse:Boolean) extends Algorithm[LineResult]{
 // println("Start: " + start + " syl cnt: " + syllableCount + " ln num: " + lineNumber + " rev: " +  reverse)
  protected var queue:Queue[BFSyllableNode] = Queue.empty[BFSyllableNode]
  
  protected var root:BFSSyllableRoot = new BFSSyllableRoot(start, (start.value.first.syllableCount +
		  						start.value.second.syllableCount))
  
  protected var nodes:ListBuffer[BFSyllableNode] = new ListBuffer[BFSyllableNode]
  
  protected var markedNodes: Set[String] = Set()
  protected var result:Option[LineResult] = None
  
  override def execute():Option[LineResult] = {
    queue.enqueue(root)

    while(!queue.isEmpty && nodes.size<4){
    	val node:BFSyllableNode = queue.dequeue
    	var pointers = node.value.forwardPointers.values
    	if(reverse){
    		 pointers = node.value.backPointers.values
    	}
    	//println("Pointers: " + pointers.size  + " for " + node + " queue size: " + queue.size)
    	pointers.foreach(f =>{
    	  var graphNode:Option[WordPairNode] = None
    	  if(!reverse){
    	     graphNode = SearchUtils.retrieveWordPairNode(node.value.value.second, f.value)
    	  }
    	  else{
    	     graphNode = SearchUtils.retrieveWordPairNode(f.value, node.value.value.first)
    	  }
    	   graphNode match{
    	    case Some(w) => {
    	    	processNode(node,w,f)
    	    }
    	    case None => {
    	      println("Could not find graph node")
    	      None
    	    }
    	  }
    	}
    	)	
      }
     result = determineResult(nodes)
     getResult
   }
  
 override def getResult():Option[LineResult] = result
  
 protected def determineResult(nodes:ListBuffer[BFSyllableNode]):Option[LineResult] = {
      
      var result = None
    //  println("Calculating for nodes: " + nodes.size)
      nodes.foreach(n => if(n.syllableCount==syllableCount)
        if(!reverse)
           this.result = Some(buildLine(new LineResult(lineNumber),n))
        else
           this.result = Some(buildLineRev(new LineResult(lineNumber),n))
     )
     this.result
  }
  
 protected def buildLine(lineResult:LineResult, node:BFSNode):LineResult = node match{
    case r: BFSSyllableRoot => {
    	lineResult.prependResult(r.value.value.second)
    	lineResult.prependResult(r.value.value.first)
    	lineResult
    	}
  	case s: BFSSyllableGraphNode => {
  	  lineResult.prependResult(s.value.value.second)
  	  buildLine(lineResult,s.backPointer)
  	  }
  }
  
 protected def buildLineRev(lineResult:LineResult, node:BFSNode):LineResult = node match{
    case r: BFSSyllableRoot => {
    	lineResult.appendResult(r.value.value.first)
    	lineResult.appendResult(r.value.value.second)
    	lineResult
    }
    case s: BFSSyllableGraphNode => {
         lineResult.prependResult(s.value.value.first)
         buildLineRev(lineResult,s.backPointer)
    }
  }
  
 protected def processNode(parent:BFSyllableNode, child:WordPairNode, newWord:WordNode) = {
         val newCount:Int = Int.box(parent.syllableCount+newWord.value.syllableCount)
         val target:Int = this.syllableCount
         if((target-newCount)==0)
    	    nodes.append(new BFSSyllableGraphNode(child,parent,newCount, newWord.probability))
    	 else if(newCount.compareTo(target)<0){
    	    if(!markedNodes.contains(child.key)){
	    	    queue.enqueue(new BFSSyllableGraphNode(child,parent,newCount, newWord.probability))
	    	    markedNodes.+=(child.key)
    	    }
    	  }
  }
 
 protected def chooseRandomResult(list:List[BFSNode]):BFSNode = {  
        if(list.size>0){
		        val rand = GenRandInt(0,(list.size/2))
				list(rand.next)
        }
        else{
           this.root
        }
  }
    
 override def getName:String = "SyllablePatternAlgorithm"
      
}

trait BFSyllableNode extends BFSNode{
  val syllableCount:Int
  def compare(that:BFSyllableNode)= {that.score compare this.score}
  override def toString():String = "BFSNode"
}

class BFSSyllableRoot(override val value:WordPairNode, val syllableCount:Int) extends BFSRoot(value) 
with BFSyllableNode{
    override def toString():String = value.toString
}

class BFSSyllableGraphNode(override val value:WordPairNode, override val backPointer:BFSNode,  val syllableCount:Int, 
   override val transitionProb:Double) extends BFSGraphNode(value,backPointer,transitionProb) with BFSyllableNode{
    override def toString():String = value.toString
}
  