package com.mickeysun.bigpaper.de

import java.io.FileWriter

import com.mickeysun.bigpaper.population._
import org.apache.spark.broadcast.Broadcast


class rddFunction(broadcastSetup:Broadcast[Setup]) extends Serializable{

    val setup = broadcastSetup.value
    val fitnessFuntion = new FitnessFuntion(setup.function,setup.chrmSize,setup.cec2005or2013)
    val de = new DE(fitnessFuntion.fitnessFuction)
    val interval = setup.migInterval
    val bound = (setup.lowerbound,setup.upperbound)


    def subPopDE(index: Int, iter: Iterator[Individual]) = {

      var generation:Int = 0
      var subPop = iter.toList
      subPop = subPop.map(x => x.setIndex(index))
      var best:Individual =  subPop.minBy(x => x.fitnessScore)

      while(generation < interval){


        //种群执行DE过程
        val deResult = de.performDE(subPop,best,bound)
        subPop = deResult._1
        best = deResult._2

        //更新代数
        generation = generation +1
      }

      //为了后续迁移进行的分区标志
      val sortsubPop = subPop.sortBy(x => x.fitnessScore)
      subPop = sortsubPop.slice(0,subPop.size-1):+ sortsubPop(0).setIndex((index+ 1) % 4)

      subPop.toIterator
    }


}


