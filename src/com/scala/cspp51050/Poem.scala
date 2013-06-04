package com.scala.cspp51050
import scala.collection.mutable.ListBuffer

trait Poem {
  var lines:ListBuffer[Line] = new ListBuffer[Line]
  val name:String
  def addLine(line:Option[Line])  = line match{
    case Some(l) => lines.append(l)
    case None =>
  }
  def printPoem(){
    println("Title: " + name)
    lines.foreach(f => f.printLine)
    println
  }
}
case class SyllablePoem(val name:String) extends Poem
case class RhymingPoem(val name:String) extends Poem
case class FreeVersePoem(val name:String) extends Poem

object PrintUtils{
  /*
def cleanLine(line:Line):Line = {
  
  checkForPlurals(line)
  checkForPunctuation(line)
}

def checkForPlurals(line:List[Word]) = line.get match{
  var result:Line = new Line(line.lineNumber)
  for(i <- line.size until 0){
    if(line.getWord(i).value.trim == "s"){
         for(j <- line.size until i++)
    }else{
       result.prependWord(line(i))
    }
  }
}

def checkForPunctuation(line:Line) = {
  
}
* *
*/

}