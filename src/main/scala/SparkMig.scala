import com.mickeysun.bigpaper.de.rddFunction
import com.mickeysun.bigpaper.population.{FitnessFuntion, Individual, Population, Setup}
import org.apache.spark.{RangePartitioner, SparkConf, SparkContext}
import org.apache.spark.broadcast.Broadcast

/**
  * Created by mickeysun on 18-5-29.
  */
object SparkMig {

  def main(args: Array[String]): Unit = {


    val setup = initConfig

    var initPop: List[Individual] = new Population(setup.numPopSize, setup.chrmSize, setup.lowerBound, setup.upperBound).initialPopulation
    val fitnessFuntion = new FitnessFuntion(setup.functionNumber, setup.chrmSize, false)
    initPop = initPop.map(x => x.setFitness(fitnessFuntion.fitnessFuction(x.chromosome)))
    val sparkConf = new SparkConf().setAppName("SparkDE")
    //.set("spark.task.cpus","3")

    sparkConf.setMaster("local")

    val sc = new SparkContext(sparkConf)

    var broadcastDE = Setup(setup.functionNumber, setup.chrmSize, 50, setup.lowerBound, setup.upperBound, false)


    var populationRDD = sc.parallelize(initPop, setup.numSubPop)
    var broadcastSetup: Broadcast[Setup] = sc.broadcast(broadcastDE)
    val DEjob = new rddFunction(broadcastSetup)

    val time1 = System.currentTimeMillis()
    for (i <- 1 to setup.maxGenerations / 50) {
      val migPop = populationRDD.mapPartitionsWithIndex(DEjob.subPopDE).map(x => (x.index, x))
      val time2 = System.currentTimeMillis()
      print("use time: " + (time2 - time1))
      populationRDD = migPop.partitionBy(new RangePartitioner(4, migPop)).map(x => x._2)
      val time3 = System.currentTimeMillis()
      print("use time: " + (time3 - time2))
      print("use time: " + (time3 - time1))
      //populationRDD.cache()

      //println(populationRDD.mapPartitions(selectBest).collect().minBy(x => x.fitnessScore).toString)
    }

  }

  def initConfig: AppConfig = {
     AppConfig("default_config.txt")
  }


}