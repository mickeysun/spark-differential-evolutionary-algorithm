import scala.io.Source


case class AppConfig(properties: Map[String, String]) extends java.io.Serializable {

  //广播出去
  val maxGenerations = properties.getOrElse("maxGenerations", "1000").toInt
  val functionNumber = properties.getOrElse("functionNumber","1").toInt
  val maxOrmin = properties.getOrElse("maxOrmin","0").toInt

  //传给population类的四个参数(only one time used)
  val numPopSize = properties.getOrElse("numPopSize", "1000").toInt
  val chrmSize = properties.getOrElse("chrmSize", "300").toInt
  val lowerBound = properties.getOrElse("lowerBound","3").toDouble
  val upperBound = properties.getOrElse("upperBound","3").toDouble

  //并行度(only one time used)
  val numSubPop = properties.getOrElse("numSubPop","4").toInt

  //有关迁移的变量
  val migrate = properties.getOrElse("migrate","10").toInt
  val threadtime = properties.getOrElse("threadtime","1000").toInt
  val a = properties.getOrElse("a","3").toInt
  val b = properties.getOrElse("b","3").toInt


}

/**
  *  This object will help to read a properties file
  */
object AppConfig {

  def apply(filename: String): AppConfig = {
    new AppConfig(AppConfig.readProperties(filename))
  }

  /**
    * Returns all the properties contained in a given file
    * @param filename
    * @return
    */
  def readProperties(filename: String): Map[String, String] = {
    var map: Map[String, String] = Map()
    val lines = Source.fromInputStream(this.getClass().getResourceAsStream(filename)).getLines()

    for (l <- lines if l.length > 0 && !l.startsWith("#") && l.contains("="))
      map += (l.split("=")(0) -> l.split("=")(1))
    map
  }
}

