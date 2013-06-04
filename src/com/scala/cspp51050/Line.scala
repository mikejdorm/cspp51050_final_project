package com.scala.cspp51050
import scala.collection.mutable.ListBuffer
class Line(val lineNumber:Int){
 private var words:ListBuffer[Word] = new ListBuffer[Word]
 def wordCounter =words.size
 def getLastSyllables:String = words(words.size-1).lastSyllables
 def getLastWord:Word = words(words.size-1)
 def appendWord(word:Word) = words.append(word)
 def prependWord(word:Word) = words.prepend(word)
 def size:Int = words.size
 def getWord(index:Int):Word = words(index)
 def getLastWordPairKey = words(words.size-1)  + "#" + words(words.size-1)
 def getLineValues:List[Word] = words.toList
 def getLineProbability:Double = this.words.foldLeft(99.0)(_ * _.probability)
 def printLine = {
   print("score: " + getLineProbability)
   print(" ")
   words.foreach(f => printToken(f.value))
   println
 }
 private def printToken(text:String) = text match{
   case x => print (x + " ")
 }
}