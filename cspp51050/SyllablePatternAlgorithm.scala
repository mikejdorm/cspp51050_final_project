package cspp51050
import scala.collection.mutable.Queue
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Set

class SyllablePatternAlgorithm(val start: WordPairNode,
  val syllableCount: Int, val lineNumber: Int, val reverse: Boolean) extends Algorithm[LineResult] {
  protected var queue: Queue[BFSyllableNode] = Queue.empty[BFSyllableNode]

  protected var root: BFSSyllableRoot = new BFSSyllableRoot(start, (start.value.first.syllableCount +
    start.value.second.syllableCount))

  protected var nodes: ListBuffer[BFSyllableNode] = new ListBuffer[BFSyllableNode]

  protected var markedNodes: Set[String] = Set()
  protected var result: Option[LineResult] = None

  override def execute(): Option[LineResult] = {
    queue.enqueue(root)
    while (!queue.isEmpty && nodes.size < 4) {
      val node: BFSyllableNode = queue.dequeue
      var pointers = node.value.forwardPointers
      if (reverse) {
        pointers = node.value.backPointers
      }
      pointers.foreach(f => {
        var graphNode: Option[WordPairNode] = None
        if (!reverse) {
          graphNode = SearchUtils.retrieveWordPairNode(node.value.value.second, f.value)
        } else {
          graphNode = SearchUtils.retrieveWordPairNode(f.value, node.value.value.first)
        }
        graphNode match {
          case Some(w) => {
            processNode(node, w, f)
          }
          case None => {
            println("Could not find graph node")
            None
          }
        }
      })
    }
    result = determineResult(nodes)
    getResult
  }

  override def getResult(): Option[LineResult] = result

  protected def determineResult(nodes:ListBuffer[BFSyllableNode]):Option[LineResult] = {
     var result = None
     var resultList:ListBuffer[LineResult] = new ListBuffer()
      nodes.foreach(n => if (n.syllableCount == syllableCount)
		   if (!reverse)
		      resultList.append(buildLine(new LineResult(lineNumber), n))
		    else
		      resultList.append(buildLineRev(new LineResult(lineNumber), n))
      	 
      )
     getMaxLine(resultList)
     
  }
  
  protected def getMaxLine(list:ListBuffer[LineResult]):Option[LineResult] = {
       var max:Option[LineResult] = None
       list.foreach(n => max match{
         case Some(n) => {
           if(n.result.getLineProbability<n.result.getLineProbability){
             max = Some(n)
           }
         }
         case None => max = Some(n)
       })
       max
  }
  protected def buildLine(lineResult: LineResult, node: BFSNode): LineResult = node match {
    case r: BFSSyllableRoot => {
      lineResult.prependResult(r.value.value.second)
      lineResult.prependResult(r.value.value.first)
      lineResult
    }
    case s: BFSSyllableGraphNode => {
      lineResult.addProbability(s.transitionProb)
      lineResult.prependResult(s.value.value.second)
      buildLine(lineResult, s.backPointer)
    }
  }

  protected def buildLineRev(lineResult: LineResult, node: BFSNode): LineResult = node match {
    case r: BFSSyllableRoot => {
      lineResult.appendResult(r.value.value.first)
      lineResult.appendResult(r.value.value.second)
      lineResult
    }
    case s: BFSSyllableGraphNode => {
      lineResult.addProbability(s.transitionProb)
      lineResult.prependResult(s.value.value.first)
      buildLineRev(lineResult, s.backPointer)
    }
  }

  protected def processNode(parent: BFSyllableNode, child: WordPairNode, newWord: WordNode) = {
    val newCount: Int = Int.box(parent.syllableCount + newWord.value.syllableCount)
    val target: Int = this.syllableCount
    if ((target - newCount) == 0)
      nodes.append(new BFSSyllableGraphNode(child, parent, newCount, newWord.probability))
    else if (newCount.compareTo(target) < 0) {
      if (!markedNodes.contains(child.key)) {
        queue.enqueue(new BFSSyllableGraphNode(child, parent, newCount, newWord.probability))
        markedNodes.+=(child.key)
      }
    }
  }

  override def getName: String = "SyllablePatternAlgorithm"
}

trait BFSyllableNode extends BFSNode {
  val syllableCount: Int
  def compare(that: BFSyllableNode) = { that.score compare this.score }
  override def toString(): String = "BFSNode"
}

class BFSSyllableRoot(override val value: WordPairNode, val syllableCount: Int) extends BFSRoot(value)
  with BFSyllableNode {
  override def toString(): String = value.toString
}

class BFSSyllableGraphNode(override val value: WordPairNode, override val backPointer: BFSNode, val syllableCount: Int,
  override val transitionProb: Double) extends BFSGraphNode(value, backPointer, transitionProb) with BFSyllableNode {
  override def toString(): String = value.toString
}
  