package com.mickeysun.bigpaper.population

import org.uma.jmetal.problem.singleobjective.CEC2005Problem
import test.FitnessFuction

/**
  * Created by mickeysun on 18-4-30.
  */
class FitnessFuntion(function:Int,chrmSize:Int,cec2005or2013:Boolean) extends Serializable{



  def fitnessFuction: (Array[Double] => Double) ={
    if(cec2005or2013 == true)  calfitnessCEC2005
    else calfitnessCEC2013
  }

  def calfitnessCEC2013(value:Array[Double]) = {
    FitnessFuction.fitness1(value,function)
  }

  def calfitnessCEC2005(value:Array[Double]) = {

      proSolution(value).getObjective(0)


  }

  def proSolution(value:Array[Double])={
    val problem: CEC2005Problem = new CEC2005Problem(function,chrmSize)

    val solution = problem.createSolution()
    for(i <- 0 to (value.size-1)){
      solution.setVariableValue(i,value(i))
    }
    problem.evaluate(solution)
    solution
  }






}
