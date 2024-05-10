import UtilsGameEngine.{Coord2D}

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

  def NumberOfWordsToFind(caminho: String): Int = {
    Source.fromFile(caminho).getLines().size
  }



}