import UtilsGameEngine.{Coord2D, HiddenWord}

import scala.io.Source

object FileManager {

  def lerPalavrasEscondidas(caminho: String): List[HiddenWord] = {

    Source.fromFile(caminho).getLines().map { linha =>
      val Array(texto, paresTexto) = linha.split(",List", 2)
      val pares = paresTexto.drop(2).dropRight(2).split("\\),\\(").toList.map {
        par =>
          val Array(x, y) = par.split(",", 2).map(_.toInt)
          (x, y)
      }
      (texto, pares)
    }.toList
  }

  def NumberOfWordsToFind(caminho: String): Int = {
    Source.fromFile(caminho).getLines().size
  }


}
