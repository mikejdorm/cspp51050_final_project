package cspp51050
import scala.xml._ 

/*
	Script to import the poetry data from an xml file and 
	then populate the wordgraph with wordnode objects and wordpair
	objects.
*/
object ImportPoetryData {

  private var graph = WordGraph
  private var dict = Dictionary
  
 def execute()={
	val elements = XML.loadFile("../poems.xml")
    (elements \ "ROW").foreach(row => parsePoem((row \ "poem_text").text.split("\\b").filter(_ != " ").toList))
    println("Graph contains " + graph.size + " nodes")
  }
  
  private def parsePoem(xs:List[String]):Unit = xs match {
      case x::y::z::Nil => {
	      var wordOne = parseWord(x.trim.toLowerCase)
	      var wordTwo = parseWord(y.trim.toLowerCase)
	      var wordThree = parseWord(z.trim.toLowerCase)
	      organizeNodes(wordOne,wordTwo,wordThree)
      }
      case x::y::z::xs => {
	      var wordOne = parseWord(x.trim.toLowerCase)
	      var wordTwo = parseWord(y.trim.toLowerCase)
	      var wordThree = parseWord(z.trim.toLowerCase)
	      organizeNodes(wordOne,wordTwo,wordThree)
	      parsePoem(y::z::xs)
      }
      case _ => println("Done")
  }
  
  private def organizeNodes(wordOne:Option[Word], wordTwo:Option[Word], wordThree:Option[Word]) = {
    	    graph.getNode(wordOne,wordTwo) match {
	        case Some(n) => wordThree match{
	          case Some(w) => n.addForwardPointer(new WordNode(w,n))
	          case None => None
	        }
	        case None => wordOne match{
	          case None => None
	          case Some(word) => wordTwo match{
	            case None => None
	            case Some(word2) => wordThree match{
	              case None => None
	              case Some(word3) => {
	                 val node = createNode(word, word2)
	                 node.addForwardPointer(new WordNode(word3,node))
	              }
	            }
	          }
	        }
	      }
    	 graph.getNode(wordTwo,wordThree) match{
    	    case Some(n) => wordOne match{
	          case Some(w) => n.addBackPointer(new WordNode(w,n))
	          case None => None
	        }
	        case None => wordTwo match{
	          case None => None
	          case Some(word2) => wordThree match{
	            case None => None
	            case Some(word3) => wordOne match{
	              case None => None
	              case Some(word) => {
	                 val node = createNode(word2, word3)
	                 node.addBackPointer(new WordNode(word,node))
	              }
	            }
	          }
	        }
    	 }
  }
  
  private def createNode(wordOne:Word, wordTwo:Word):WordPairNode = {
    val node = new WordPairNode(new WordPair(wordOne, wordTwo))
    WordGraph.addEntry(node)
    node
  }
  
  private def parseWord(text:String):Option[Word] = text match{
    case "&#39;" => {
      None
    }
    case "&amp" => {
      None
    }
    case tabbedRegex(_) => {
      None
    }
    case puncRegex2(_) => {
      None
    }
    case lineBreakRegex(_) => {
      None
    }
    case wordRegex(_) => {
      bindWordToDictionaryEntry(text)
    }
    case _ => {
      None
    }
  }
  val tabbedRegex = "[??]*".r
  val puncRegex2 = ".*(\\p{Punct})".r
  val wordRegex = ".*(\\w)$".r
  val puncRegex = ".*(\\W)$".r
  val lineBreakRegex = "\n".r
  val vowelRegex = "[aeiou]".r

  private def cleanPunctuation(text:String):Word = {
     new Word(text,text)
  }
  
  private def cleanLineBreak(text:String):Word = {
     new Word(text,text)
  }
	
  private def bindWordToDictionaryEntry(text:String):Option[Word] = {
     dict.getWord(text.toLowerCase()) match {
       case Some(w) => {
          Some(new Word(text,w.syllables)) }
       case None => None
     }
  }
  
  private def guessSyllables(text:String) = {
    new Word(text,text)
  }
    
}
