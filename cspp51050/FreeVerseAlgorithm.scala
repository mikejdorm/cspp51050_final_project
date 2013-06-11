package cspp51050

class FreeVerseAlgorithm(val lines:Int, seed:WordPairNode) extends Algorithm[PoemResult[FreeVersePoem]] {
  // println("Creating free verse poem with " + lines + " lines and a seed node of " + seed)
   val graph = WordGraph
   private var poem:FreeVersePoem = new FreeVersePoem(seed.value.first + " " + seed.value.second)
   private var result = new PoemResult[FreeVersePoem](poem)
   val boundry:Int = GenRandInt(6,12).next
   
   override def execute():Option[PoemResult[FreeVersePoem]] = {
         var lineSeed:WordPairNode = seed
         var diff = GenRandInt(0, 6)
         for(i <- 0 until lines){
            var lineResult:Line = createLine(i,lineSeed, diff.next)
            result.appendResult(lineResult)
            graph.getNode(lineResult.getLastWordPairKey) match{
              case Some(node) => lineSeed = getNextSeed(node)
              case None => lineSeed = getRandomSeed
            }
         }
         Some(result)
   }
   
   private def createLine(lineNum:Int, seed:WordPairNode, diff:Int):Line = {
        var lineSeed = seed 
        var count:Int = 0
        var line:Option[LineResult] = None
        while(line==None && count<=100){
          line = new SyllablePatternAlgorithm(lineSeed, (boundry-diff), lineNum, false).execute 
          lineSeed = getRandomSeed
          count = count + 1
        }
        if(count==100){
          throw new Error
        }
        line match {
          case Some(l)=> l.result
          case None => throw new Error
        }
   }
   
   private def getRandomSeed():WordPairNode = {
       WordGraph.entries.values.toList(GenRandInt(1, graph.size-1).next) 
   }

   private def getNextSeed(node:WordPairNode):WordPairNode = {    
        if(node.forwardPointers.size>=1){
          val result = getRandomNode(node.forwardPointers.values.toList, node.value.second)
          getRandomNode(result.forwardPointers.values.toList, result.value.second)
        }
        else if(node.backPointers.size>=1){
        	 getRandomSeed
        }  
        else{
          getRandomSeed
        }
   }
/*
   private def getNextSeed(node:WordPairNode):WordPairNode ={
       if(node.forwardPointers.size>=1){
          graph.getNode(Some(node.value.second), 
              Some(node.forwardPointers.values.toList(0).value)) match{
            case Some(pair) => {
               getPopularNode(node.forwardPointers.values.toList,pair, node.value.second)
            }
            case None =>{
              getRandomSeed
            }
          }
       }
       else{
    	   getRandomSeed
       }
   }
   * 
   */
   
   private def getPopularNode(words:List[WordNode], max:WordPairNode, word:Word):WordPairNode = words match{
     case x::xs =>   graph.getNode(Some(word), Some(words(0).value)) match{
           				 case Some(pair) => {
           				   if(pair.forwardPointers.size> max.forwardPointers.size)
           					 getPopularNode(words,pair,word)
           				   else
           				     getPopularNode(words,max,word)
           				 }
           				 case None => getPopularNode(xs,max,word)
     				}
     case _ => max
   }
   private def getRandomNode(words:List[WordNode], word:Word):WordPairNode ={
      if(words.size ==1){   
           graph.getNode(Some(word), Some(words(0).value)) match{
           case Some(pair) => pair
           case None => getRandomNode(words,word)
         }
      }
      else if(words.size>1){
         val rand = GenRandInt(0,words.size-1).next
         graph.getNode(Some(word), Some(words(rand).value)) match{
           case Some(pair)=> pair
           case None => getRandomNode(words,word)
         }
      }
      else{
        getRandomSeed
      }
      
   }
   
    override def getResult:Option[PoemResult[FreeVersePoem]] = Some(result)
        
    override def getName:String = "FreeVerseAlgorithm"
}
