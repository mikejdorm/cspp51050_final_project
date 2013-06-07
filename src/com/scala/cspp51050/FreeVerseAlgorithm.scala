package com.scala.cspp51050
/*
class FreeVerseAlgorithm(val lines:Int, seed:WordPairNode) extends Algorithm[PoemResult[FreeVersePoem]] {
   val graph = WordGraph
   private var poem:FreeVersePoem = new FreeVersePoem(seed.value.first + " " + seed.value.second)
   private var result = new PoemResult[FreeVersePoem](poem)
   override def execute():PoemResult[FreeVersePoem] = {
     println("Using seed: " + seed.key + " with " + seed.counter + " transitions") 
      var current:WordPairNode = seed
      for(i <- 1 to lines){
        val line:Line = buildLine(current,poem,i)
        graph.getNode(line.getLastWordPairKey) match{
          case Some(n) => current = n
          case None => println("End of poem")
        } 
      } 
     	return result
   }
   override def getResult:PoemResult[FreeVersePoem] = result
        
   def buildLine(seed:WordPairNode, poem:Poem, lineNum:Int):Line = {
     
     var line:Line = new Line(lineNum)
     val randomNum:Int = 10
     var current:WordPairNode = seed
     for(i <- 1 to randomNum){
       line.appendWord(current.value.first)
       line.appendWord(current.value.second)
       val newWord:WordNode = current.getRandomForwardTransition
       line.appendWord(newWord.value)
       graph.getNode(seed.value.second.value+"#"+newWord.value.value) match{
         case Some(n) => current = n
         case None => println("End of the line")
       }
     }
     return line
     
   }
  override def getName:String = "FreeVerseAlgorithm"
}
*/