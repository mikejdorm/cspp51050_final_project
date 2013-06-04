package com.scala.cspp51050
import scala.collection.mutable.ListBuffer
object UserInterface {

   def generateRandomPoem():Poem = {
      val rnd = new GenRandInt(1, WordGraph.entries.size-1)
      var root:WordPairNode = WordGraph.entries.values.toList(rnd.next) 
      var poem:FreeVersePoem = new FreeVersePoem(root.value.first.value + " " + root.value.second.value)
      for(i <- 1 to 10){
        var bfs:BreadthFirstSearch = new BreadthFirstSearch(root, 5)
        bfs.execute
        var current:Option[BFSNode]= bfs.getResult 
        poem.addLine(SearchUtils.createLine(current, i))
        current match{
          case Some(b)=> root = b.value
          case None => 
        }
      }
      return poem
   }
   
   def generateRandomSyllablePoem():SyllablePoem = {
      val rnd = new GenRandInt(1, WordGraph.entries.size-1)
      var root:WordPairNode = WordGraph.entries.values.toList(rnd.next) 
      var poem:SyllablePoem = new SyllablePoem(root.value.first.value + " " + root.value.second.value)
        var bfs:SyllablePatternAlgorithm = new SyllablePatternAlgorithm(root, 17)
        bfs.execute
        var current:Option[BFSNode]= bfs.getResult 
        poem.addLine(SearchUtils.createLine(current, 1))
      return poem
   }
   
   def generateRandomIambicPentameterPoem():RhymingPoem = {
        new RhymingPoem("Holder")
     
   }
  
   def generateLimerick():RhymingPoem = {
       new RhymingPoem("holder")
   }
   
   def generateHaiku():SyllablePoem = {
        new SyllablePoem("Holder")
   }
  
  
}