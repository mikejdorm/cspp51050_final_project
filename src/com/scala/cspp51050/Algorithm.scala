package com.scala.cspp51050

trait Algorithm[X <: AlgorithmResult[_,_]] {
 def execute():Option[X]
 def getName():String
 def getResult():Option[X]
}

trait AlgorithmResult[X,C]{
  var result:X
  def appendResult(c:C)
  def prependResult(c:C)
}
case class LineResult(val lineNumber:Int) extends AlgorithmResult[Line,Word]{
       override var result = new Line(lineNumber)
       override def appendResult(word:Word) =  result.appendWord(word)
       override def prependResult(word:Word) = result.prependWord(word)
}
case class WordResult(word:Word) extends AlgorithmResult[Word,Word]{ 
   override var result = word
   override def appendResult(word:Word) = this.result = word
   override def prependResult(word:Word) = result = word
}
case class PoemResult[X <: Poem](override var result:X) extends AlgorithmResult[X,Line]{
	  override def appendResult(line:Line) = result.appendLine(Some(line))
	  override def prependResult(line:Line) = result.prependLine(Some(line))
}
