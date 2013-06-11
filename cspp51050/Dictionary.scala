package cspp51050
import scala.collection.mutable.HashMap

/*
The Dictionary object is a singleton that holds the phonetic
dictionary parsed from a text file. The word values are held
in a hash map and can be retreived through the methods getWord(index)
or getWord(text). 

The Dictionary also contains a reference to words that should not 
end a sentence and in most cases should also not end a line. These 
words are also supplied by a text file and include conjunctions, prepositions, etc. 
*/

object Dictionary {
  
	 private var dictionary:HashMap[String,Word] = new HashMap[String,Word]
	 
	 def getWord(key:String):Option[Word]  =  dictionary.get(key.trim.toLowerCase())
	 
	 def getWord(indx:Int):Option[Word] = if(indx<dictionary.size) Some(dictionary.values.toList(indx)) else None
	 
	 def addWord(word:Word) = dictionary.put(word.value,word)
	 
	 def size():Int = dictionary.size
	 
	 def clear() = dictionary.clear
	 
	 def containsWord(key:String):Boolean = dictionary.contains(key)
	 
	 private var nonEndings:HashMap[String,Word] = new HashMap[String, Word]
	 def isNonLineEnder(word:Word):Boolean = nonEndings.get(word.value) match{
	   case Some(w) => true
	   case None => false
	 }
	 def addNonLineEnder(word:Word) = nonEndings.put(word.value,word)
	 def nonLineEndingsSize:Int = nonEndings.size
	 def iterator:DictionaryIterator = new DictionaryIterator(dictionary.values.toList)
	 
}

/*
Iterator object for the Dictionary Singleton
*/
class DictionaryIterator(val list:List[Word]){
   private var _current = 0
   def current:Option[Word] = if(_current<list.size) Some(list(_current)) else None
   def next:Option[Word] = {
	   if(_current+1<list.size) {
	     _current = _current + 1 
	     Some(list(_current))
	   }
	   else{
	     None
	   }
   }
   def hasNext:Boolean = _current+1<list.size
  
  
}

/*
  Script to import the non line ending words into the dictionary
*/
object ImportNonLineEndingWords{
  
  def readFile() = {
	  scala.io.Source.
	  fromFile("./conj_preps_puncs.txt").getLines().
	  foreach {   line => parseLine(line)}
	  println("non line endings initailized with: "  + Dictionary.nonLineEndingsSize)
  }
  
  private def parseLine(line:String) = {
      if(Dictionary.containsWord(line.trim.toLowerCase)){
         Dictionary.getWord(line.trim.toLowerCase) match{
           case Some(word)=>{
             Dictionary.addNonLineEnder(word)
           } 
           case None => None
         }
      }
  }
}

/*
Script to import the words into the phonetic dictionary singleton
*/
object ImportPhoneticDictionary {
  
  val dictionary =  Dictionary
  
  def readFile() = {
  scala.io.Source.fromFile("./cmudict.0.7a").getLines().foreach {   line => parseLine(line)}
  println("dictionary initailized with: "  +dictionary.size + " words")
 }
  
 private def parseLine(line:String) = line.split(" ").toList match{
    case ";;;"::Nil => println
    case a => parseItMore(a)
  }

 private def parseItMore(line:List[String]) = line match{
    case x::xs => parsePhonetic(cleanWord(x),xs)
    case _ => println()
  }
  
  private def cleanWord(word:String):String = {
    if(word.contains("(")){
      return word.substring(0, word.indexOf("("))
    }	
    else{
      return word
    }
  }
  
  private def parsePhonetic(word:String, line:List[String]) = {
	  dictionary.addWord(new Word(word.toLowerCase(), line.filter(l => l != "") mkString ", "))
  }
  
}
