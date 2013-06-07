package com.scala.cspp51050
import scala.collection.mutable.ListBuffer

trait Component 
trait Composite[X <: Component] extends Component{
   protected val components:ListBuffer[X] = new ListBuffer
}

trait Poem extends Composite[Line]{
  val name:String
  def addLine(line:Option[Line])  = line match{
    case Some(l) => components.append(l)
    case None =>
  }
  def appendLine(line:Option[Line]) = line match{
    case Some(l) => components.append(l)
    case None => None
  }
  def prependLine(line:Option[Line]) = line match{
    case Some(l) => components.prepend(l)
    case None => None
  }
  def printPoem(){
    println("Title: " + name)
    components.foreach(f => f.printLine)
    println
  }
  def getLine(lineNumber:Int):Option[Line] = {
    if(lineNumber >= 0 && lineNumber < components.size){
      Some(components(lineNumber))
    }
    else
    	None
  }
  def getLineCount:Int = components.size
}

case class SyllablePoem(val name:String) extends Poem

case class RhymingPoem(val name:String) extends Poem

case class FreeVersePoem(val name:String) extends Poem

class Line(val lineNumber:Int) extends Composite[Word]{
 def wordCounter =components.size
 def getLastSyllables:String = components(components.size-1).lastSyllables
 def getLastWord:Word = components(components.size-1)
 def appendWord(word:Word) = components.append(word)
 def prependWord(word:Word) = components.prepend(word)
 def size:Int = components.size
 def getWord(index:Int):Word = components(index)
 def getLastWordPairKey:String = {
   if(components.size >= 2){
	 components(components.size-2)  + "#" + components(components.size-1)}
   else
	""
}
 def getLineValues:List[Word] = components.toList
 def getLineProbability:Double = this.components.foldLeft(99.0)(_ * _.probability)
 def printLine = {
  // print("score: " + getLineProbability)
   //print(" syl: " + (for(i <- components) yield i.syllableCount))
   print(" ")
   components.foreach(f => printToken(f.value))
   println
 }
 override def toString():String =(for(i <- components) yield i.toString + " ").mkString
 private def printToken(text:String) = text match{
   case x => print (x + " ")
 }
}

class Word(val value:String, val syllables:String) extends Component{
  val numbers = Array('1','2','3','4','5','6','7','8','9','0')
  
  def syllableCount:Int = {
    if(syllables.split(numbers).size-1 == 0) 1 
    else syllables.split(numbers).size-1
  }
  def lastSyllables():String = syllables.split(numbers).last
  var probability:Double = 0.0
  def setProbability(value:Double) = {this.probability = value}
  override def toString():String = value
  
}