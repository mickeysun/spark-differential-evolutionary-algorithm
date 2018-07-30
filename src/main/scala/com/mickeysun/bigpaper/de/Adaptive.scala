package com.mickeysun.bigpaper.de

import java.io.PrintWriter

import breeze.stats.distributions.Gaussian
import com.mickeysun.bigpaper.population.Individual

import scala.util.Random

/**
  * Created by mickeysun on 18-4-25.
  */
object Adaptive {

  def adaptive(bestPop: Array[Individual],writer1: PrintWriter,writer2: PrintWriter): Array[Individual] = {


     var tmp:Array[Double] = bestPop.map(x => Math.abs(x.bestfitness.get(24) - x.bestfitness.get(0)))

     bestPop




  }



  def stateflag(bestind: Individual):Int = {
     var state = 0

     val  bestfitness = bestind.bestfitness.get

     val tmp = Math.abs(bestfitness(24) - bestfitness(0)) < 1.0 * Math.pow(10, -6)
    // println("-----fitness change--------" + Math.abs(bestfitness(24) - bestfitness(0)))
     val spd = spdfunction(bestind.spd.get)


       if (tmp) {
         if(spd){
            state = 2
         }else{
           state = 1
         }
       }
    //println("---------" + state)
       state

  }

  def spdfunction(arr:Array[Double]): Boolean ={
      var count = 0
      for(i <- 1 to arr.length-1){
         val tmp = Math.abs(arr(i) - arr(i-1))
         if(tmp < 1){
            count = count + 1
         }
      }
      if(count > arr.length / 2){
         false
      }else{
        true
      }
  }

  def adaptiveParameter(subPop:List[Individual],gen:Int) = {
    var subpop = subPop
    val weightOfSubpop = weight(subPop.map(x => x.fitnessScore.get))
    val p = 0.8 - 0.45 * Math.pow((1-gen),2)
    val size =  weightOfSubpop.size
    var weightF:Double = 0
    var weightCR:Double = 0
    for(i <- 0 to size-1){
      weightCR = weightCR + weightOfSubpop(i) * subPop(i).FandCR._1
      weightF = weightF + weightOfSubpop(i) * subPop(i).FandCR._2
    }
    val Flist = new Gaussian(weightF,p).sample(size)
    val CRlist = new Gaussian(weightCR,p).sample(size)
    for(i <- 0 to size -1){
       subpop = subpop.updated(i,subpop(i).setFandCR(Flist(i),CRlist(i)))
    }
    subpop


  }

  def weight(fitnessOfSubpop:List[Double]):List[Double] = {

    var sumfitness:Double= 0
    val bestfitness:Double = fitnessOfSubpop.max

    var weightVector:List[Double] = List()
    for(i <- 0 to fitnessOfSubpop.size-1){
      sumfitness += Math.abs(fitnessOfSubpop(i)- bestfitness)
    }
    for(i <- 0 to fitnessOfSubpop.size -1){
      weightVector = weightVector :+ (Math.abs(fitnessOfSubpop(i)- bestfitness) / sumfitness)
    }
    weightVector
  }






}
