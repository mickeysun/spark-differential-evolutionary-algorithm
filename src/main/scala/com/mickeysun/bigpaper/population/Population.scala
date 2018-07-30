package com.mickeysun.bigpaper.population

import java.util.concurrent.ThreadLocalRandom

import scala.collection.mutable.ListBuffer
import scala.util.Random

/**
  * Created by mickeysun on 18-4-13.
  */
class Population(popSize: Int,
                 chrmSize: Int,
                 lowerBound:Double,
                 upperBound:Double) extends Serializable{



  def initialPopulation: List[Individual] = {
    val population = new ListBuffer[Individual]

    for (i <- 1 to popSize) {
      population += generateIndividual
    }
    population.toList

  }

  def generateIndividual: Individual = {
    val r = new Random()

    var chromsome: Array[Double] = Array()
    for (i <- 1 to chrmSize) {


      val number = ThreadLocalRandom.current().nextDouble(lowerBound,upperBound)


      chromsome =  chromsome :+ number

    }
    new Individual(
                   -1,
                   chromsome,
                   Option.empty[Double],
                    3,
                  (0.5,0.9),
                     0,
                  Option.empty[Array[Double]],
                  Option.empty[Array[Double]],
                  Option.empty[Array[Double]])
  }

}
