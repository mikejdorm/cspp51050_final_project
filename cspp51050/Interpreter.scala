package cspp51050


/*

     Simple interpreter object to read in commands
     from the command prompt and execute functions 
     from the user interface.
*/
object Interpreter {

     def readCommand(command:String) = command.toLowerCase.split(" ").toList match{
       case "new"::"limerick"::Nil =>  UserInterface.generateLimerick.printPoem
       case "new"::"haiku"::Nil => UserInterface.generateHaiku.printPoem
       case "new"::"freeverse"::Nil => UserInterface.generateFreeVersePoem.printPoem
       case "new"::"freeverse"::x::Nil => UserInterface.generateFreeVersePoem(x.toInt).printPoem
       case "info"::xs::Nil => getNodeInfo(xs)
       case "help"::Nil => printInstructions()
       case _ => println("invalid command")
     }
     
     private def getNodeInfo(commands:String) = commands.split(" ").toList match{
       case x::y::Nil => UserInterface.getNodeInfo(x,y)
       case x::Nil => UserInterface.getWordInfo(x)
       case _ => println("invalid command")
     }
  
     private def printInstructions(){
       println("Supported Commands")
       println("new limerick - Generates a random limerick with ABABCCA rhyme pattern")
       println("new haiku - Generates a random English Limerick")
       println("new freverse - Generates a random free verse poem")
       println("new freeverse # - Generates a random free verse poem with the given amount of lines")
       println("info x - Retrieves info on a given word x")
       println("info x y - Retrieves info on a given word x/word y node")
     }
  
}
