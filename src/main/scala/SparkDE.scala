import java.io.FileWriter

import com.mickeysun.bigpaper.de.rddFunction
import com.mickeysun.bigpaper.population.{Population, _}
import org.apache.spark.broadcast.Broadcast
import org.apache.spark.{RangePartitioner, SparkConf, SparkContext}
/**
  * Created by mickeysun on 17-7-21.
  *
  * 现在是最新版本的带迁移操作的DDE实现
  */

object SparkDE {



  def main(args: Array[String]): Unit = {


    val setup = initConfig

    var initPop: List[Individual] = new Population(setup.numPopSize, setup.chrmSize, setup.lowerBound, setup.upperBound).initialPopulation
    val fitnessFuntion = new FitnessFuntion(setup.functionNumber,setup.chrmSize,false)
    initPop = initPop.map(x => x.setFitness(fitnessFuntion.fitnessFuction(x.chromosome)))
    val sparkConf = new SparkConf().setAppName("SparkDE")

    sparkConf.setMaster("local")

    val sc = new SparkContext(sparkConf)

    val broadcastDE = Setup(setup.functionNumber, setup.chrmSize, 50,setup.lowerBound,setup.upperBound, false)


    var populationRDD = sc.parallelize(initPop, setup.numSubPop)
    val broadcastSetup: Broadcast[Setup] = sc.broadcast(broadcastDE)
    val DEjob = new rddFunction(broadcastSetup)


    val writer = new FileWriter("/home/mickeysun/1.txt")

    for(i <- 1 to setup.maxGenerations / 50){

       val migPop = populationRDD.mapPartitionsWithIndex(DEjob.subPopDE).map(x => (x.index+1,x))


       populationRDD = migPop.partitionBy(new RangePartitioner(4,migPop)).map(x => x._2)


       val best = populationRDD.mapPartitions(select,preservesPartitioning = true).collect().minBy(x => x.fitnessScore.get)

       writer.write(best.fitnessScore.get + "\n")



    }
    writer.close()


  }

  def select(iter:Iterator[Individual])={
     val name = iter.toList
     val name1 = name.head
     //println(name1)
     List(name1).toIterator

  }





  def initConfig:AppConfig={


     AppConfig("default_config.txt")
  }




}
