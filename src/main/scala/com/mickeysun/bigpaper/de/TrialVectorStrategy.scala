package com.mickeysun.bigpaper.de

import scala.util.Random
/**
  * Created by mickeysun on 18-4-13.
  */
class TrialVectorStrategy(bound:(Double,Double)) extends Serializable{

  def mutatebound(value:Double)={
     if(value < bound._1) bound._1
     else if(value > bound._2) bound._2
     else value
  }

  def de_rand_1(targetVector:Array[Double],
                F:Double,
                CR:Double,
                randomVectors:List[Array[Double]]):Array[Double] = {


    var trialVector:Array[Double] = Array()
    val chrmSize = targetVector.size
    val randJ = Random.nextInt(chrmSize)
    //crossover vector
    // (mutationVector,targetVector) => trialvector
    for(i <- 0 to chrmSize -1){


      if(i == randJ || Random.nextDouble() <= CR){
        var mutateI = randomVectors(0)(i) + F * (randomVectors(1)(i) - randomVectors(2)(i))
        mutateI = mutatebound(mutateI)
        trialVector = trialVector :+ mutateI
      }
      else trialVector = trialVector :+ targetVector(i)
    }

    return trialVector

  }

  def de_rand_2(targetVector:Array[Double],
                F:Double,
                CR:Double,
                randomVectors:List[Array[Double]]):Array[Double] = {
    //println("haha" + "CR---" + CR)
    var trialVector:Array[Double] = Array()
    val chrmSize = targetVector.size
    val randJ = Random.nextInt(chrmSize)
    //crossover vector
    // (mutationVector,targetVector) => trialvector
    for(i <- 0 to chrmSize -1){

      if(i == randJ || Random.nextDouble() <= CR){
        var mutateI = randomVectors(0)(i)
        + F * (randomVectors(1)(i) - randomVectors(2)(i))
        + F * (randomVectors(3)(i) - randomVectors(4)(i))

        mutateI = mutatebound(mutateI)

        trialVector = trialVector :+ mutateI
        //println("----- " + mutateI + "----" + i)
      }
      else trialVector = trialVector :+ targetVector(i)
    }
    //println("mama" + targetVector.mkString(" "))
    return trialVector

  }

  def de_curTorand_2(targetVector:Array[Double],
                F:Double,
                CR:Double,
                randomVectors:List[Array[Double]]):Array[Double] = {


    var trialVector:Array[Double] = Array()
    val chrmSize = targetVector.size
    val K = Random.nextDouble()
    //crossover vector
    // (mutationVector,targetVector) => trialvector
    for(i <- 0 to chrmSize -1){
      var mutateI = targetVector(i)
                   + F * (randomVectors(0)(i) - targetVector(i))
                   + F * (randomVectors(1)(i) - randomVectors(2)(i))

      mutateI = mutatebound(mutateI)

      trialVector = trialVector :+ mutateI
    }

    return trialVector

  }

  def de_randTobest_2(targetVector:Array[Double],
                      bestVector:Array[Double],
                      F:Double,
                      CR:Double,
                      randomVectors:List[Array[Double]]):Array[Double] = {


    var trialVector:Array[Double] = Array()
    val chrmSize = targetVector.size
    val randJ = Random.nextInt(chrmSize)
    val K = Random.nextDouble()
    //crossover vector
    // (mutationVector,targetVector) => trialvector
    for(i <- 0 to chrmSize -1){

      if(i == randJ || Random.nextDouble() <= CR){
        var mutateI =  targetVector(i) +
          K * (bestVector(i) - targetVector(i))
         F * (randomVectors(0)(i) - randomVectors(1)(i) + randomVectors(2)(i) - randomVectors(3)(i))
        mutateI = mutatebound(mutateI)

        trialVector = trialVector :+ mutateI

      }
      else trialVector = trialVector :+ targetVector(i)
    }

    return trialVector

  }
  def de_curTobest_2(targetVector:Array[Double],
                      bestVector:Array[Double],
                      F:Double,
                      CR:Double,
                      randomVectors:List[Array[Double]]):Array[Double] = {


    var trialVector:Array[Double] = Array()
    val chrmSize = targetVector.size
    val randJ = Random.nextInt(chrmSize)
    val K = Random.nextDouble()
    //crossover vector
    // (mutationVector,targetVector) => trialvector
    for(i <- 0 to chrmSize -1){

      if(i == randJ || Random.nextDouble() <= CR){
        var mutateI =  targetVector(i) +
          F * (bestVector(i) - targetVector(i))
        F * (randomVectors(0)(i) - randomVectors(1)(i) + randomVectors(2)(i) - randomVectors(3)(i))
        mutateI = mutatebound(mutateI)
        trialVector = trialVector :+ mutateI
      }
      else trialVector = trialVector :+ targetVector(i)
    }

    return trialVector

  }



  def de_best_1(targetVector:Array[Double],
                bestVector:Array[Double],
                F:Double,
                CR:Double,
                randomVectors:List[Array[Double]]):Array[Double] = {


    var trialVector:Array[Double] = Array()
    val chrmSize = targetVector.size
    val randJ = Random.nextInt(chrmSize)
    //crossover vector
    // (mutationVector,targetVector) => trialvector
    for(i <- 0 to chrmSize -1){

      if(i == randJ || Random.nextDouble() <= CR){
        var mutateI = bestVector(i) + F * (randomVectors(0)(i) - randomVectors(1)(i))
        trialVector = trialVector :+ mutateI
        mutateI = mutatebound(mutateI)
      }
      else trialVector = trialVector :+ targetVector(i)
    }

    return trialVector

  }


}
