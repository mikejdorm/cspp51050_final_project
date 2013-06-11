package cspp51050
import scala.collection.mutable.Queue
import scala.collection.mutable.ListBuffer

object SearchUtils{
  def retrieveWordPairNode(word:Word,word2:Word):Option[WordPairNode] = {
     WordGraph.getNode(word.value.trim + "#" + word2.value.trim)
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
}

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
  
  