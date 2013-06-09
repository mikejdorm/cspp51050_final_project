package com.scala.cspp51050
import scala.collection.mutable.ListBuffer
object UserInterface {
   
  private val graph = WordGraph
  
  def getWordInfo(word:String) = {
      Dictionary.getWord(word) match{
        case Some(w) => {
              println("Word: " + w.value)
              println("Syllables: " + w.syllables)
              println("Syllable Count: " + w.syllableCount)
        	  println("Probability: " + w.probability)
        }
        case None => println("Word not found")
      }
      
  }
  
  def getNodeInfo(word1:String, word2:String) = {
    
  }

   def generateRandomIambicPentameterPoem():RhymingPoem = {
        new RhymingPoem("Holder")
   }
   
   def generateFreeVersePoem():FreeVersePoem = {
       val rnd = new GenRandInt(1, WordGraph.entries.size-1)
       val seed = WordGraph.entries.values.toList(rnd.next)
       val rnd2 = new GenRandInt(4,12)
      try{
	      new FreeVerseAlgorithm(rnd2.next,seed).execute match{
	         case Some(poem) => poem.result
	         case None => generateFreeVersePoem
	       } 
      }
       catch{
         case e: Error => generateFreeVersePoem
       }  
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
   
   def generateSyllablePoem(lineSyllables:List[Int]) = {
     
   }
   
   def generateRhymingPoem(lineSyllables:List[Int], rhymingPattern:List[Int]) = {
     
   }
   
   def generateFreeVersePoem(lines:Int):FreeVersePoem = {
       val rnd = new GenRandInt(1, WordGraph.entries.size-1)
       val seed = WordGraph.entries.values.toList(rnd.next)
      try{
	      new FreeVerseAlgorithm(lines,seed).execute match{
	         case Some(poem) => poem.result
	         case None => generateFreeVersePoem
	       } 
      }
       catch{
         case e: Error => generateFreeVersePoem
       }  
   }
  
}