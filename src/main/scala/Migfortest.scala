import com.mickeysun.bigpaper.population.{Individual, Population}
import org.apache.spark.{RangePartitioner, SparkConf, SparkContext}
import test.FitnessFuction


object Migfortest {


  def main(args: Array[String]): Unit = {

    val setup = initConfig

    val initPop: List[Individual] = new Population(setup.numPopSize, setup.chrmSize, setup.lowerBound, setup.upperBound).initialPopulation

    val sparkConf = new SparkConf().setAppName("SparkDE")


    sparkConf.setMaster("local")

    val sc = new SparkContext(sparkConf)

    val populationRDD = sc.parallelize(initPop, setup.numSubPop)




  }




  def initConfig: AppConfig = {

    AppConfig("default_config.txt")
  }

}
