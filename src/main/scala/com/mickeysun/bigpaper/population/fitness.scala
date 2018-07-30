package com.mickeysun.bigpaper.population

trait Fitness extends java.io.Serializable {

  def fitnessFunction(vector: Vector[Double]): Double
}



class FitnessF1 extends Fitness with java.io.Serializable {

  override def fitnessFunction(vector: Vector[Double]) = {
       var result = 0.0

       for(number <- vector){
          result += Math.pow(number,2.0)
       }
       result

  }
}

class FitnessF2 extends Fitness with java.io.Serializable {

  override def fitnessFunction(vector: Vector[Double]) = {

    var sum = 0.0
    var multi = 1.0
    for (number <- vector) {
       sum += Math.abs(number)
       multi = multi * Math.abs(number)

    }
    sum + multi
  }
}

class FitnessF3 extends Fitness with java.io.Serializable {

  override def fitnessFunction(vector: Vector[Double]) = {
    val arr = vector
    var sum = 0.0

    for (i <- 0 to arr.length-1) {
       var tmp = 0.0
       for(j <- 0 to i){
          tmp += arr(j)
       }
       sum += Math.pow(tmp,2.0)
    }
    sum
  }
}
class FitnessF4 extends Fitness with java.io.Serializable {

  override def fitnessFunction(vector: Vector[Double]) = {
    val arr = vector
    var max = 0.0

    for (i <- 0 to arr.length-1) {
      val num = Math.abs(arr(i))
      if(num > max) max = num
    }
    max
  }
}

class FitnessF5 extends Fitness with java.io.Serializable {

  override def fitnessFunction(vector: Vector[Double]) = {
    var result = 0.0

    for(number <- vector){
      val tmp = Math.floor(number + 0.5)
      result += Math.pow(tmp,2.0)
    }
    result
  }
}

class FitnessF6 extends Fitness with java.io.Serializable {

  override def fitnessFunction(vector: Vector[Double]) = {
    val arr = vector
    var sum = 0.0

    for (i <- 0 to arr.length-1) {
      sum +=( (i+1) * Math.pow(arr(i),4.0))
    }
    sum += scala.util.Random.nextDouble()//有问题是[0,1）
    sum
  }
}
class FitnessF7 extends Fitness with java.io.Serializable {

  override def fitnessFunction(vector: Vector[Double]) = {
    val arr = vector
    var sum = 0.0

    for (i <- 0 to arr.length-2) {
      val tmp1 = Math.pow((arr(i)-1),2.0)
      val tmp2 = 100 * Math.pow((arr(i+1) - Math.pow(arr(i),2.0) ),2.0)
      sum += (tmp1 + tmp2)

    }
    sum
  }
}

class FitnessF8 extends Fitness with java.io.Serializable {

  override def fitnessFunction(vector: Vector[Double]) = {
    val arr = vector
    var sum = 0.0
    val length = arr.length
    val tmp1 = length * 418.9829
    for (number <- arr) {

     sum += number * Math.sin(Math.sqrt(Math.abs(number)))

    }
    tmp1 - sum
  }
}


class FitnessF9 extends Fitness with java.io.Serializable {

  override def fitnessFunction(vector: Vector[Double]) = {
    val arr = vector
    var sum = 0.0

    for (number <- arr) {
      sum += (Math.pow(number,2.0) - 10 * Math.cos(2*Math.PI*number) + 10)
    }
    sum
  }
}


class FitnessF10 extends Fitness with java.io.Serializable {

  override def fitnessFunction(vector: Vector[Double]) = {
    val arr = vector
    var sumF = 0.0
    var sumC = 0.0

    for (number <- arr) {
      sumF += Math.pow(number,2.0)
      sumC += Math.cos(2*Math.PI*number)
    }
    -20 * Math.exp(-0.2* Math.sqrt(sumF / arr.length)) - Math.exp(sumC / arr.length) + 20 + Math.E
  }
}
class FitnessF11 extends Fitness with java.io.Serializable {

  override def fitnessFunction(vector: Vector[Double]) = {
    val arr = vector
    var sum = 0.0
    var multi = 1.0

    for (i <- 0 to arr.length-1) {
      sum += Math.pow(arr(i),2.0)
      multi = multi * (Math.cos(arr(i) / Math.sqrt(Math.abs(arr(i)))))
    }
    sum / 4000.toDouble - multi + 1
  }
}

class FitnessF12 extends Fitness with java.io.Serializable {

  override def fitnessFunction(vector: Vector[Double]) = {
    val arr = vector
    val last = arr.length-1
    var sum1 = 0.0
    var sum2 = 0.0

    for (i <- 0 to arr.length-2) {
      sum1 += Math.pow(getY(arr(i)) - 1,2.0) * (1 + 10 * Math.pow(Math.sin(Math.PI * getY(arr(i+1))),2.0))
      sum2 += getU(arr(i),10,100,4)
    }
    sum2 += getU(arr(last),10,100,4)
    sum1 += Math.pow(getY(arr(last))-1,2.0)
    sum1 += 10 * Math.pow(Math.sin(Math.PI * getY(arr(0))),2.0)
    sum2 += (sum1) * (Math.PI  /arr.length.toDouble)
    sum2
  }

  def getY(x:Double): Double ={
    1 + (x +1.0) / 4.toDouble
  }

  def getU(x:Double,a:Int,k:Int,m:Int):Double={
     var res = 0.0
     if(x > a) res = k * Math.pow((x - a),m)
     else if(x < -a) res = k * Math.pow((-x - a),m)
     res
  }
}
class FitnessF13 extends Fitness with java.io.Serializable {

  override def fitnessFunction(vector: Vector[Double]) = {
    val arr = vector
    val last = arr.length-1
    var sum1 = 0.0
    var sum2 = 0.0

    for (i <- 0 to arr.length-2) {
      sum1 += Math.pow(arr(i) - 1,2.0) * (1 +  Math.pow(Math.sin(3*Math.PI * arr(i+1)),2.0))
      sum2 += getU(arr(i),5,100,4)
    }
    sum1 += Math.pow(Math.sin(3*Math.PI*arr(0)),2.0)
    sum1 += Math.pow(arr(last)-1,2.0)*(1 + Math.pow(Math.sin(2*Math.PI*arr(last)),2.0))
    sum2 += (0.1 * sum1)
    sum2
  }



  def getU(x:Double,a:Int,k:Int,m:Int):Double={
    var res = 0.0
    if(x > a) res = k * Math.pow((x - a),m)
    else if(x < -a) res = k * Math.pow((-x - a),m)
    res
  }
}







