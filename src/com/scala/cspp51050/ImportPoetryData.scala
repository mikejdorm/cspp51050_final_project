package com.scala.cspp51050
import scala.xml._ 
object ImportPoetryData {

  private var graph = WordGraph
  private var dict = Dictionary
  
 def execute()={
	val elements = XML.loadFile("/Users/michaeldorman/Desktop/poems.xml")
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
	          case None =>
	        }
	        case None => wordOne match{
	          case None => 
	          case Some(word) => wordTwo match{
	            case None => 
	            case Some(word2) => wordThree match{
	              case None => 
	              case Some(word3) => {
	                 val node = createNode(word, word2)
	                 node.addForwardPointer(new WordNode(word3,node))
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
      Some(new Word(text,text))
    }
    case lineBreakRegex(_) => {
      Some(cleanLineBreak(text))
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