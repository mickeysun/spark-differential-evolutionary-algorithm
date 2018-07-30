package com.mickeysun.bigpaper.population

/**
  * Created by mickeysun on 18-4-14.
  */
object HPD{

  def weight(subPop:List[Individual]):Vector[Double] = {
    var sumfitness:Double= 0
    var weightVector:Vector[Double] = Vector()
    for(i <- 0 to subPop.size-1){
      sumfitness += subPop(i).fitnessScore.get
    }
    for(i <- 0 to subPop.size -1){
      weightVector = weightVector :+ subPop(i).fitnessScore.get / sumfitness
    }
    weightVector
  }
  def calculateHPD_mean(weightVector:Vector[Double],subPop:List[Individual])={

    val subPopGene = subPop.map(x => x.chromosome)
    val w0:Double=  weightVector(0)
    var aveGene = subPopGene(0).map(x => x * w0 )

    for(i <- 1 to subPop.size-1){
      val w = weightVector(i)
      for(j <- 0 to aveGene.size-1){
        val tmp = aveGene(j) + w * subPop(i).chromosome(j)
        aveGene = aveGene.updated(j,tmp)
      }


    }

    aveGene
  }

  def calculateHPD_std(subPop:List[Individual]):(Array[Double],Array[Double]) = {
    val weightVector = weight(subPop)
    val size = subPop.size
    val aveGene = calculateHPD_mean(weightVector,subPop)
    var stdGene = aveGene
    for(i <- 0 to aveGene.size-1){
      val tmp = aveGene(i)
      var sum:Double = 0
      for(j <- 0 to size-1){
        sum += weightVector(j) * Math.pow(subPop(j).chromosome(i)-tmp,2)
      }
      stdGene = stdGene.updated(i,sum)
    }
    (aveGene,stdGene)
  }

  def calculatePopDiversity(subPop:List[Individual])={
    val tmp = calculateHPD_std(subPop)
    val size = tmp._1.size
    var sum:Double = 0
    for(i <- 0 to size-1){
      sum +=  tmp._2(i) / tmp._1(i)
    }

    sum/size
  }

}

object SPD{

  def calculateSPD_mean(subPop:List[Individual])={
    val subPopGene = subPop.map(x => x.chromosome)

    var aveGene = subPop(0).chromosome

    for(i <- 1 to subPop.size-1){
      for(j <- 0 to aveGene.size-1){
        val tmp = aveGene(j) + subPop(i).chromosome(j)
        aveGene = aveGene.updated(j,tmp)
      }

      if(i == subPop.size-1) aveGene.map(x => x / subPop.size)

    }

    aveGene
  }

  def calculateSPD_std(subPop:List[Individual]):(Array[Double],Array[Double]) = {
    val size = subPop.size
    val aveGene = calculateSPD_mean(subPop)
    var stdGene = aveGene
    for(i <- 0 to aveGene.size-1){
      val tmp = aveGene(i)
      var sum:Double = 0
      for(j <- 0 to size-1){
        sum += Math.pow(subPop(j).chromosome(i)-tmp,2)
      }
      stdGene = stdGene.updated(i,sum /size)
    }
    (aveGene,stdGene)
  }

  def calculatePopDiversity(subPop:List[Individual])={
    val tmp = calculateSPD_std(subPop)
    val size = tmp._1.size
    var sum:Double = 0
    for(i <- 0 to size-1){
      sum +=  tmp._2(i) / tmp._1(i)
    }

    sum/size
  }
}
