package com.scala.cspp51050

trait Algorithm {
 def execute()
 def getName():String
}

abstract class AlgorithmResult[X](val result:X)
case class LineResult(override val result:Line) extends AlgorithmResult[Line](result)
case class WordResult(override val result:Word) extends AlgorithmResult[Word](result)
case class PoemResult(override val result:Poem) extends AlgorithmResult[Poem](result)
  
