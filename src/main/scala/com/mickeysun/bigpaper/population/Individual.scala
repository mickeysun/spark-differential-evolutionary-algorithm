package com.mickeysun.bigpaper.population

/**
  * Created by mickeysun on 17-7-21.
  */

//定义Individual
//后期根据需要进行字段增加与修改
case class Individual(index:Int,
                      chromosome:Array[Double],

                      fitnessScore: Option[Double],
                      mutation:Int,
                      FandCR:(Double,Double),
                      state:Int,
                      bestfitness:Option[Array[Double]],
                      spd:Option[Array[Double]],
                      hpd:Option[Array[Double]]
                     ) extends Serializable{




  def setIndex(value:Int):Individual = copy(index = value)

  def setChrom(value:Array[Double]) = copy(chromosome = value)

  def setFitness(value:Double) = copy(fitnessScore = Some(value))

  def setMutation(value:Int) = copy(mutation = value)

  def setState(value:Int) = copy( state = value)

  def setFandCR(value:(Double,Double)) = copy(FandCR = value)




  override def toString: String =
   // "{\"chromosome\": ["+ chromosome.toArray.mkString(",") + "]"+
      "\n\"index\": " + index +
      ",\n\"fitnessScore\": " + fitnessScore.get
     // ",\n\"spd\": [" + spd.get.mkString(",") + "]" +
      //",\n\"hpd\": [" + hpd.get.mkString(",") + "]"



}









