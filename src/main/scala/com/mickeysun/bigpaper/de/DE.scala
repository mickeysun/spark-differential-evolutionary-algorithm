package com.mickeysun.bigpaper.de

import com.mickeysun.bigpaper.population.{HPD, Individual, SPD}

import scala.util.Random

/**
  * Created by mickeysun on 18-4-30.
  */
class DE(f:(Array[Double] => Double)) extends Serializable{



  def performDE(subPop:List[Individual],best1:Individual,bound:(Double,Double)):(List[Individual],Individual) ={

    var best = best1
    val curbest:Individual = best
    var nextsubPop:List[Individual] = List.empty
    val TrialVectorStrategy = new TrialVectorStrategy(bound)

    for (i <- 0 to subPop.size-1) {

      val target = subPop(i)
      val strategy = target.mutation
      val FandCR = target.FandCR
      val F = FandCR._1
      val CR = FandCR._2

      strategy match{
        case 3  =>  {
          val randomVecotrs = randomSelection(i, 3, subPop)
          val tmp = selection(target,TrialVectorStrategy.de_rand_1(target.chromosome,F,CR, randomVecotrs))
          nextsubPop = nextsubPop :+ tmp

        }
        case 4 => {
          val randomVecotrs = randomSelection(i, 5, subPop)
          val tmp = selection(target,TrialVectorStrategy.de_rand_2(target.chromosome,F, CR, randomVecotrs))
          nextsubPop = nextsubPop :+ tmp
        }
        case 1 => {
          val randomVecotrs = randomSelection(i, 2, subPop)
          val tmp =  selectionbest(target,TrialVectorStrategy.de_best_1(target.chromosome,curbest.chromosome, F, CR,randomVecotrs),best)
          nextsubPop = nextsubPop :+ tmp._1
          best = tmp._2
          //println(best.fitnessScore)
        }
        case 2 => {
          val randomVecotrs = randomSelection(i, 4, subPop)
          val tmp =  selectionbest(target,TrialVectorStrategy.de_curTobest_2(target.chromosome,curbest.chromosome, F, CR,randomVecotrs),best)
          nextsubPop = nextsubPop :+ tmp._1
          best = tmp._2

        }

      }
    }
    (nextsubPop,best)
  }

  def selection(target:Individual,trialVector:Array[Double]): Individual ={

    var trialFitnessScore = f(trialVector)

    var resultIndividual = target

    if(trialFitnessScore < target.fitnessScore.get) {
      resultIndividual = target.setChrom(trialVector).setFitness(trialFitnessScore)
    }
    resultIndividual

  }



  def selectionbest(target:Individual,trialVector:Array[Double],best:Individual): (Individual,Individual)={

    var trialFitnessScore = f(trialVector)




    var resultIndividual = (target,best)

    if(trialFitnessScore < target.fitnessScore.get) {
      val nextIndividual  = target.setChrom(trialVector).setFitness(trialFitnessScore)
      resultIndividual = (nextIndividual,best)
      if(trialFitnessScore < best.fitnessScore.get) resultIndividual = (nextIndividual,nextIndividual)
    }



    resultIndividual

  }




  def randomSelection(targetIndex: Int,randomSize: Int,subPop:List[Individual]) = {

    var result: List[Array[Double]] = List.empty;


    var indexList = Map.empty[Int, Boolean]
    for (i <- 0 to subPop.size - 1) {
      indexList += (i -> true)
    }


    var cur_size = randomSize
    while (cur_size > 0) {
      val index = Random.nextInt(subPop.size)
      //println("index " + index + "pos " + pos)
      if(indexList(index) == true && index != targetIndex) {
        result = result :+ subPop(index).chromosome
        cur_size = cur_size - 1
        //println(size1)
        indexList += (index -> false)
      }
    }
    result
  }

  def performDiversity(subPop:List[Individual],generation:Int,best:Individual): List[Individual] ={

    val spd = SPD.calculatePopDiversity(subPop)
    val hpd = HPD.calculatePopDiversity(subPop)
    val fitness = best.fitnessScore.get

    subPop.map(x => x.spd.get(generation) = spd)
    subPop.map(x => x.hpd.get(generation) = hpd)
    subPop.map(x => x.bestfitness.get(generation) = fitness)
    subPop

  }
}
