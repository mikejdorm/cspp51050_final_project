package cspp51050
import scala.collection.mutable.HashMap

class Graph{
  
}

/*
The WordGraph object holds a directed graph of the words 
parsed from the poetry inputs. The WordGraph includes methods
for adding and connecting nodes. The nodes in the WordGraph
are words pairs of words that connect to Word objects in order to
store the transitions from two words to the next, so (x,y) -> z. 
Word transitions are also back pointing as well to accommodate the
rhyming scheme algorithm so the wordgraph also stores transtions
 a -> (b,c). The object also includes methods for retreiving word node 
 objects.
*/
object WordGraph extends Graph{
  
  var entries:HashMap[String,WordPairNode] = new HashMap[String, WordPairNode]
  
  def addEntry(node:WordPairNode) = entries.put(node.key, node)
  
  def size:Int = entries.size
  
  protected def addForwardPointerToNode(key:String, word:WordNode) = {
    println("Adding forward pointer: "  + word.key +  " to " + key)
     entries.get(key) match{
       case Some(x) => x.addForwardPointer(word)
       case None => println("interesting")
     }
  }
  
  def addBackPointerToNode(key:String, word:WordNode) = {
    entries.get(key) match{
      case Some(x) => x.addBackPointer(word)
      case None => println("ok")
    }
  }
  
  def addForwardPointerToNode(key:String, word:Word) = {
    entries.get(key) match{
      case Some(x) => x.addForwardPointer(new WordNode(word,x,false))
      case None => println("ok")
    }
  }
  
  def addBackPointerToNode(key:String, word:Word) ={
    entries.get(key) match{
      case Some(x) => x.addBackPointer(new WordNode(word,x,true))
      case None => println("")
    }
  }
  
  def getNode(word1:Option[Word], word2:Option[Word]):Option[WordPairNode] = word1 match{
    case Some(x) => word2 match{
      case Some(y) => entries.get(x.value + "#" + y.value)
      case None => None
    }
    case None => None
  }
  
  def getNode(key:String):Option[WordPairNode] = entries.get(key)
  
}
