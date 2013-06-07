package com.scala.cspp51050
import scala.collection.mutable.ListBuffer
object UserInterface {
   
   def generateRandomIambicPentameterPoem():RhymingPoem = {
        new RhymingPoem("Holder")
   }
   
   def gnerateFreeVersePoem():FreeVersePoem = {
          new FreeVersePoem("Holder")
   }
  
   def generateLimerick():RhymingPoem = {
       val rnd = new GenRandInt(1, WordGraph.entries.size-1)
       val seed = WordGraph.entries.values.toList(rnd.next)
       val algo =  new RhymeSchemeAlgorithm(List(6,6,4,4,6), List(1,1,2,2,1),
           seed.value.first + " " + seed.value.second, seed)
       try{
          algo.execute match {
            case Some(poem) => poem.result
            case None => throw new Error
          }
       }
       catch{
         case e: Error => generateLimerick
       }
   }
   
   def generateHaiku():RhymingPoem = {
       val rnd = new GenRandInt(1, WordGraph.entries.size-1)
       val seed = WordGraph.entries.values.toList(rnd.next)
       
       val algo =  new RhymeSchemeAlgorithm(List(5,7,5), List(1,2,3),
           seed.value.first + " " + seed.value.second, seed)
       try{
          algo.execute match {
            case Some(poem) => poem.result
            case None => throw new Error
          }
       }
       catch{
         case e: Error => generateHaiku
       }
   }
   
   

  
}