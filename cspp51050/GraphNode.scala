package cspp51050
import scala.collection.mutable.HashMap

/*
  Abstract GraphNode object the object held
  by the graph node is taken as a parameter. 
  The purpose of this class is mainly to require
  a key/value for each graph node object.
*/
abstract class GraphNode[X] extends Component{
  def key:String
  val value:X
}

trait Counter{
  var _count:Int = 0
  def increment = {_count = _count  + 1}
  def count:Int = _count
  override def toString:String = "Counter: " + count
}

case class BaseCounter extends Counter{
  override def toString:String = "BaseCounter: " + count
}
case class ForwardCounter extends Counter{
    override def toString:String = "ForwardCounter: " + count
}
case class BackCounter extends Counter{
    override def toString:String = "BackCounter: " + count
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
   private var _counter:BaseCounter = new BaseCounter
   private var _forwardCounter:ForwardCounter = new ForwardCounter
   private var _backCounter:BackCounter = new BackCounter
   
  //creates a key to reference the word pair node.
  override def key:String = value.first.value+"#"+value.second.value
 
  
  private var _forwardPointers:HashMap[String,WordNode] = new HashMap[String,WordNode]
  private var _backPointers:HashMap[String,WordNode] = new HashMap[String,WordNode]
  def backCounter:Int = _backCounter.count
  def forwardCounter:Int = _forwardCounter.count
  def masterCounter() = _counter.count
  
  /*
    This is a general function to add a word node to the passed hash
    map object. This is used by the addForwardPointer and addBackPointer
    functions. 
  */
  
  def backPointers:List[WordNode] = _backPointers.values.toList
  def forwardPointers:List[WordNode] = _forwardPointers.values.toList
  private def addPointer(map:HashMap[String, WordNode], x:WordNode, counter:Counter) = {
    if(map.contains(x.value.value)){
      map.get(x.value.value) match{
        case Some(x) => {
         incrementCounter(counter)
         x.incrementCount()
        }
        case None => None
      }
    }
    else{
      incrementCounter(counter)
      x.incrementCount()
      map.put(x.key,x)
    }
  }
  private def incrementCounter(counter:Counter) = { 
    counter match{
         case f: ForwardCounter => _forwardCounter.increment
		 case b: BackCounter => _backCounter.increment
		 case c: BaseCounter => _counter.increment 
		 case _ => None
  }
     println("Incrementing: "+ counter)
  }
  //add a forward pointer
  def addForwardPointer(x:WordNode) =  addPointer(_forwardPointers,x,_forwardCounter)

  //add a back pointer
  def addBackPointer(x:WordNode) = addPointer(_backPointers,x,_backCounter)

  override def toString():String = key
  
  //currently not used but this should return a random forward pointer
  //when implemented.
  def getRandomForwardTransition():WordNode = _forwardPointers.values.last //TODO add random function
  
}

/*
  WordPair node class to bind to word objects
*/
class WordPair(val first:Word, val second:Word)
  
/*
  WordNode object to hold a word and also to bind to the parent object.
*/
case class WordNode(val value:Word, val parent:WordPairNode,val backPointer:Boolean) extends GraphNode[Word]{
     var counter:Int = 0
     override def key:String = value.value
     def incrementCount() = {this.counter = this.counter +1 }
     
     def probability:Double = {
       if(backPointer && parent.backCounter!=0){
        // println(value + " probability: " + this.counter.toDouble+"/"+parent.backCounter.toDouble + " " +  this.counter.toDouble/parent.backCounter.toDouble)
         this.counter.toDouble/parent.backCounter.toDouble
       }
       else if(!backPointer && parent.forwardCounter!=0){
        // println(value + " probability: " +  this.counter.toDouble + "/" +parent.forwardCounter.toDouble + " " + this.counter.toDouble/parent.forwardCounter.toDouble)
         this.counter.toDouble/parent.forwardCounter.toDouble
       }
       else{
         //println(value + "No probability")
         0.0
       }
     } 
     
  }
  
