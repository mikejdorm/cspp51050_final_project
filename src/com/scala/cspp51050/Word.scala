package com.scala.cspp51050

class Word(val value:String, val syllables:String){
  val numbers = Array('1','2','3','4','5','6','7','8','9','0')
  def syllableCount:Int = syllables.split(numbers).size
  def lastSyllables():String = syllables.split(numbers).last
  var probability:Double = 0.0
  def setProbability(value:Double) = {this.probability = value}
}