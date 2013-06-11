package cspp51050
import scala.collection.mutable.HashMap

/*
  Abstract GraphNode object the object held
  by the graph node is taken as a parameter. 
  The purpose of this class is mainly to require
  a key/value for each graph node object.
*/
abstract class GraphNode[X] {
  def key:String
  val value:X
}

/*
  The WordPairNode holds a pair of words and the holds refrences
  to word nodes that are the transitions to and from the WordPair
  
  example:
      The poem text "Charge of the Light" will be a WordPair{"of", "the"} with
      a back pointer to WordNode{"Charge"} and a forward pointer to WordNode{"Light"}. 

*/
case class WordPairNode(val value:WordPair) extends GraphNode[WordPair]{
  /*
    counter to track the word transtions
  */
  var counter:Int = 0
  
  //creates a key to reference the word pair node.
  override def key:String = value.first.value+"#"+value.second.value
 
  var forwardPointers:HashMap[String,WordNode] = new HashMap[String,WordNode]
  var backPointers:HashMap[String,WordNode] = new HashMap[String,WordNode]


  def incrementMasterCounter() = {this.counter = this.counter + 1}
  
  /*
    This is a general function to add a word node to the passed hash
    map object. This is used by the addForwardPointer and addBackPointer
    functions. 
  */
  private def addPointer(map:HashMap[String, WordNode], x:WordNode) = {
    if(map.contains(x.value.value)){
      map.get(x.value.value) match{
        case Some(x) => {
          this.incrementMasterCounter()
          x.incrementCount()
        }
        case None => None
      }
    }
    else{
        this.incrementMasterCounter()
          x.incrementCount()
      map.put(x.key,x)
    }
  }

  //add a forward pointer
  def addForwardPointer(x:WordNode) =  addPointer(forwardPointers,x)

  //add a back pointer
  def addBackPointer(x:WordNode) = addPointer(backPointers,x)

  override def toString():String = key
  
  //currently not used but this should return a random forward pointer
  //when implemented.
  def getRandomForwardTransition():WordNode = forwardPointers.values.last //TODO add random function
  
}

/*
  WordPair node class to bind to word objects
*/
class WordPair(val first:Word, val second:Word)
  
/*
  WordNode object to hold a word and also to bind to the parent object.
*/
case class WordNode(val value:Word, val parent:WordPairNode) extends GraphNode[Word]{
     var counter:Int = 0
     override def key:String = value.value
     def incrementCount() = {this.counter = this.counter +1 }
     def probability:Double = {
       if(parent.counter != 0.0){
         value.setProbability(this.counter/parent.counter)
       }
        value.probability
     } 
     
  }
  
