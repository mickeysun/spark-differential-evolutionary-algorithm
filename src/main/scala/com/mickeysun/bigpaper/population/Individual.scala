package com.mickeysun.bigpaper.population

/*
 * Created by mickeysun on 17-7-21.
 */

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

  override def toString: String =
   // "{\"chromosome\": ["+ chromosome.toArray.mkString(",") + "]"+
      "\n\"index\": " + index +
      ",\n\"fitnessScore\": " + fitnessScore.get
     // ",\n\"spd\": [" + spd.get.mkString(",") + "]" +
      //",\n\"hpd\": [" + hpd.get.mkString(",") + "]"
}









