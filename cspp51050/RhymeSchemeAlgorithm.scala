package cspp51050
import scala.collection.mutable.Queue
import scala.collection.mutable.ListBuffer

class RhymeSchemeAlgorithm(val lineSyllables:List[Int], 
    val rhymingPattern:List[Int], val name:String,
    val seedWord:WordPairNode) extends Algorithm[PoemResult[RhymingPoem]]{
  
  protected  var poem:RhymingPoem = new RhymingPoem(name)
  protected  val result:PoemResult[RhymingPoem]  = PoemResult[RhymingPoem](poem)
  protected  var rhymes:ListBuffer[String] = new ListBuffer[String]
  
/*
 * Builds a rhyming poem given the arguments. If a poem cannot
 * be created a None object is returned or an Error is thrown. 
 */
 override def execute():Option[PoemResult[RhymingPoem]]  ={
       var seed:WordPairNode = seedWord
	   for(i <- 0 until lineSyllables.size){
	     getSeedValue(i) match{
	       case Some(s) => seed = s
	       case None => {
	         if(i== 0) seed = seed else seed = getRandomSeed()
	       }
	     }
	     buildLine(seed,lineSyllables(i),i) match{
	       case Some(l) => {
	              result.appendResult(l)
	              rhymes.append(l.getLastWord.value)
	              WordGraph.getNode(l.getLastWordPairKey) match{
	              	case Some(s) => seed = s
	              	case None => seed = seed
	              }
	       }
	       case None => {
	         throw new Error
	       }
	     }
	   }
      Option(result)
 }
/*
 *    Returns a random word pair node from the graph.
 */    
 protected def getRandomSeed():WordPairNode = {
       val rnd = new GenRandInt(1, WordGraph.entries.size-1)
       WordGraph.entries.values.toList(rnd.next)
 }
 
 /*
  * Gets a rhyming seed value if it needs to rhyme with a prior line
  * gets a random seed value if it doesn't have to rhyme and it's not
  * the first line. If it's the first line it uses the seed passed to
  * the algorithm. 
  */
 protected def getSeedValue(lineNumber:Int):Option[WordPairNode] = {
		 if(lineNumber==0){
		     Some(seedWord)
		 }
		 else{
	         result.result.getLine(rhymingPattern.dropRight(rhymingPattern.size-lineNumber)
	             .lastIndexOf((rhymingPattern(lineNumber))))
	            match{
	           case Some(line) => {
	           		WordGraph.getNode(line.getLastWordPairKey)
	           }
	           case None =>{ 
	           	 None
	           }
	         }
		 }
 }
 
override def getResult():Option[PoemResult[RhymingPoem]] = Some(result)
 
 /*
  * Builds a line given a seed value and the amount of syllables in the line. 
  * returns a line if possible. None if a line cannot be built with the 
  * given parameters.
  */
protected def buildLine(seed:WordPairNode,lineSyllables:Int, lineNumber:Int):Option[Line] = {
        if(lineNumber == 0){
           new SyllablePatternAlgorithm(seed,lineSyllables, lineNumber, true).execute match{
             case Some(v) => Some(v.result)
             case None => None
           }
        }
        else{
		 findRhyme(seed) match{
		   case None => None
		   case Some(s) => new SyllablePatternAlgorithm(s,lineSyllables, lineNumber, true).execute match{
		     case Some(v) => Some(v.result)
		     case None => None
		   }
		   
		 }
        }
 }

 /*
  * Finds a rhyme. First checks nearby nodes. If nothing is
  * found then it searches for one in the WordGraph 
  */
 protected def findRhyme(word:WordPairNode):Option[WordPairNode] = {
	  var queue:Queue[WordPairNode] = Queue.empty[WordPairNode]
	  var markedNodes: Set[String] = Set()
	  var found:Boolean = false
	  var result:WordPairNode = word
	  queue.enqueue(word)
	  while(!queue.isEmpty && !found){
		    var check = queue.dequeue
		    if(rhymes(check.value.second,word.value.second)){
		       found = true
		       result = check
		    }
		    else{
		        word.forwardPointers.foreach(f => {
		    	WordGraph.getNode(Some(word.value.second), Some(f.value)) match {
		    	  case Some(w) => { 
		    	    if(!markedNodes.contains(w.key)){
		    	    queue.enqueue(w)
		    	    markedNodes.+=(w.key)
		    	    }
		    	  }
		    	  case _ => None
		    	}
		    }	
		    )
		 }
	  }
	  if(result != word){
	     Option(result)
	  }
	  else{
	     findRhymeInWordGraph(word)
	  }
 }
 /*
  * Tail recursive function to find a rhyming word. returns a word pair node
  * if a rhyme is found. None if nothing is found. 
  */
 protected def findRhymeInWordGraph(word:WordPairNode):Option[WordPairNode] = {
		 val list:List[WordPairNode] = WordGraph.entries.values.toList
		 def findRhymeInWordGraphAux(word:WordPairNode, wordList:List[WordPairNode]):Option[WordPairNode] = wordList match{
		   case x::xs =>{ if(word.value.second != x.value.second
		       && rhymes(word.value.second, x.value.second)){
		    	   Some(x)
		 	  }	
		     else{
		       findRhymeInWordGraphAux(word,xs)
		     }
		   }
		   case _ => None
		 }
		 findRhymeInWordGraphAux(word,list)
 }
/*
 * Returns true if the word rhymes false otherwise
 * 
 * The function takes two words and checks if they rhyme.
 * The function checks this by splitting each words syllable
 * string and then compares the two syllable lists. If each
 * word has the same syllable at the same index then it is 2 points
 * if the words have the same syllables in close proximity to eachother
 * then it's 1 point. The final score is then compared against each
 * words size to make sure it's a relatively close fit. This last check
 * is basically to make sure each word is relatively the same in size and
 * that the score applies to word one and word two. 
 */
 protected def rhymes(wordOne:Word, wordTwo:Word):Boolean = {
     if(wordOne.value==wordTwo.value || rhymes.contains(wordTwo.value)){
        false
     }
     else{
	     val syl1 = wordOne.syllables.split(" ")
	     val syl2 = wordTwo.syllables.split(" ")
	     var counter:Int = 1
	     var score:Int =0;
	     syl2.foreach(s => 
	       if(syl1.indexOf(s)>0){
	         val prox =  Math.abs(syl1.indexOf(s)-syl2.indexOf(s))
	         if(prox==0 && syl1.lastIndexOf(s)>(syl1.size/2))
	           score+=2
	         else if(prox<=1){
	           score+=1
	         }   
	       }
	       )
	     if(score/syl1.size>.50 && score/syl2.size>.50){
	       true
	     }
	     else{
	       false
	     }
     }
 }

 override def getName:String = "RhymeSchemeAlgorithm"
}






