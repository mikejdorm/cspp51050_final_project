package com.scala.cspp51050


object Main {
  
  def main(args: Array[String]):Unit = { 
      ImportPhoneticDictionary.readFile
      ImportPoetryData.execute
      ImportNonLineEndingWords.readFile
      Dictionary.clear
      println
      for(i <- 1 to 10){
	      UserInterface.generateHaiku.printPoem
	      UserInterface.generateLimerick.printPoem
      }
  }
  
}
