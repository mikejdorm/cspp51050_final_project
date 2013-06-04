package com.scala.cspp51050


object Main {
  def main(args: Array[String]):Unit = { 
      ImportPhoneticDictionary.readFile
      ImportPoetryData.execute
      Dictionary.clear
      println
      for(i <- 1 until 100)
       UserInterface.generateRandomPoem.printPoem


     // val poem = UserInterface.generateRandomSyllablePoem
     // poem.printPoem
  }
  
}
