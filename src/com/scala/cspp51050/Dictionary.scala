package com.scala.cspp51050
import scala.collection.mutable.HashMap
object Dictionary {
 private var dictionary:HashMap[String,Word] = new HashMap[String,Word]
 def getWord(key:String):Option[Word]  = dictionary.get(key)
 def addWord(word:Word) = dictionary.put(word.value,word)
 def size():Int = dictionary.size
 def clear() = dictionary.clear
 def containsWord(key:String):Boolean = dictionary.contains(key)
}

object ImportPhoneticDictionary {
    //import java.sql.{Connection, DriverManager, ResultSet};
	//  val conn_str = "jdbc:mysql://localhost:3306/poet_tree?user=root&password="
	//  val conn = DriverManager.getConnection(conn_str)
	 val dictionary =  Dictionary
  def readFile() = {
  scala.io.Source.fromFile("/Users/michaeldorman/Desktop/poet_tree_project/cmudict/cmudict.0.7a").getLines().foreach {   line => parseLine(line)}
  println("dictionary initailized with: "  +dictionary.size + " words")
 }
  
  def parseLine(line:String) = line.split(" ").toList match{
    case ";;;"::Nil => println
    case a => parseItMore(a)
  }

  def parseItMore(line:List[String]) = line match{
    case x::xs => parsePhonetic(cleanWord(x),xs)
    case _ => println()
  }
  def cleanWord(word:String):String = {
    if(word.contains("(")){
      return word.substring(0, word.indexOf("("))
    }	
    else{
      return word
    }
  }
  def parsePhonetic(word:String, line:List[String]) = {
 // val statement = conn.createStatement(ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)
 // val prep = conn.prepareStatement("INSERT INTO phonetic_dictionary (word, syllables) VALUES (?, ?) ")
//  prep.setString(1, word.toLowerCase())
 // prep.setString(2,  line.filter(l => l != "") mkString ", ")
 // prep.executeUpdate
   dictionary.addWord(new Word(word.toLowerCase(), line.filter(l => l != "") mkString ", "))
  }
}