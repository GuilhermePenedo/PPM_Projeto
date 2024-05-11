
import UtilsGeneral.Coord2D

import java.io.{File, PrintWriter}
import scala.io.Source

object FileManager {

  def lerPalavrasEscondidas(path: String): List[(String, List[Coord2D])] = {
    val bufferedSource = Source.fromFile(path)
    try {
      val hiddenWords = for {
        line <- bufferedSource.getLines()
        Array(palavra, valoresCoordenadas) = line.split(",List", 2)
        coordenadas = valoresCoordenadas.drop(2).dropRight(2).split("\\),\\(").toList.map {
          coordenada =>
            val Array(x, y) = coordenada.split(",", 2).map(_.toInt)
            (x, y)
        }
      } yield (palavra, coordenadas)
      hiddenWords.toList
    } finally {
      bufferedSource.close()
    }
  }

  def readRandom(): Int = {
    val bufferedSource = Source.fromFile("random.txt")
    try {
      val hiddenWords = for {
        line <- bufferedSource.getLines()
      } yield line
      hiddenWords.toList.head.toInt
    } finally {
      bufferedSource.close()
    }
  }

  def writeRandom(number: Int): Unit = {
    val writer = new PrintWriter(new File("random.txt"))
    try {
      writer.println(number)
    } finally {
      writer.close()
    }
  }

  def NumberOfWordsToFind(caminho: String): Int = {
    Source.fromFile(caminho).getLines().size
  }



}