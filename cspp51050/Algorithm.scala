package cspp51050
/*
Algorithm trait to provide a template
for all algorithm implementation. The 
parameters passed to the AlgorithmResult are the
result type and the object wrapped by the result.
*/
trait Algorithm[X <: AlgorithmResult[_,_]] {
 def execute():Option[X]
 def getName():String
 def getResult():Option[X]
}

/*
Algorithm result to provide a standard
result component to be returned by the algorithm.
*/
trait AlgorithmResult[X,C]{
  var result:X
  def appendResult(c:C)
  def prependResult(c:C)
}

/*
LineResult returns a line object.
*/
case class LineResult(val lineNumber:Int) extends AlgorithmResult[Line,Word]{
       override var result = new Line(lineNumber)
       override def appendResult(word:Word) =  result.appendWord(word)
       override def prependResult(word:Word) = result.prependWord(word)
       def addProbability(prob:Double) = result.addProbabilityValue(prob)
}
/*
WordResult wraps a Word object
*/
case class WordResult(word:Word) extends AlgorithmResult[Word,Word]{ 
   override var result = word
   override def appendResult(word:Word) = this.result = word
   override def prependResult(word:Word) = result = word
}

/*
PoemResult wraps a poem object
*/
case class PoemResult[X <: Poem](override var result:X) extends AlgorithmResult[X,Line]{
	  override def appendResult(line:Line) = result.appendLine(Some(line))
	  override def prependResult(line:Line) = result.prependLine(Some(line))
}
