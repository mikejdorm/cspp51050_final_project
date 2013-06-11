package cspp51050


object Main {
  
  def main(args: Array[String]):Unit = { 
      ImportPhoneticDictionary.readFile
      ImportPoetryData.execute
      ImportNonLineEndingWords.readFile
      println
      /*
      for(i <- 1 to 1000){
	      UserInterface.generateHaiku.printPoem
	      Thread.sleep(4000)
       // UserInterface.generateLimerick.printPoem
	      //UserInterface.generateLimerick.printPoem
      }
      * 
      */
      
      println("Enter a command to begin or \"help\" for a list of commands...\n")
       for( ln <- io.Source.stdin.getLines ) Interpreter.readCommand(ln)
      println("Dictionary: " + Dictionary.size)
      Dictionary.getWord(1) match{
        case Some(word) =>  UserInterface.getWordInfo(word.value)
        case None => println("none")
      }
     
      
  }
  
}
