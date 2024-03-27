trait Random {
  def nextInt: (Int, Random)
}
case class MyRandom(seed: Long) extends Random {
  def nextInt: (Int, Random) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) &
      0xFFFFFFFFFFFFL
    val nextRandom = MyRandom(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRandom)
  }
}

object Main {

  def randomChar(rand:MyRandom):(Char, MyRandom) = {
    val r = rand.nextInt
    // A = 65  e Z = 90
    val n = (r._1 % (25))
    val c = (if(n<0) (-n + 65) else (n + 65))
    (c.toChar, MyRandom(r._1))
  }



  def main(args: Array[String]): Unit = {
    val c = randomChar(MyRandom(2))
    val c1 = randomChar(c._2)
    val c2 = randomChar(c1._2)
    val c3 = randomChar(c2._2)
    val c4 = randomChar(c3._2)
    val c5 = randomChar(c4._2)
    System.out.println(c._1, c1._1, c2._1, c3._1, c4._1, c5._1)

  }
}