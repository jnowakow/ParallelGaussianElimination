package Parsing

import scala.io.Source
import java.io.{BufferedWriter, File, FileWriter}

object MatrixParser {
  type Matrix = Array[Array[Double]]
  type Row = Array[Double]

  def Row(xs: Double*): Row = Array(xs: _*)

  def Matrix(xs: Row*): Matrix = Array(xs: _*)

  /**
   * Wczytuje macierz z pliku w formacie:
   * rozmiar macierzy
   * wiersze
   * wektor odpowiedzi
   * np:
   * 2
   * 2.0 0.0
   * 0.0 2.0
   * 4.0 4.0
   * Zwraca macierz rozszerzoną, tzn o wymiarach NxN+1 gdzie ostatina kolumna to macierz odpowiedzi.
   * Dla powyższego wejścia będzie to macierz
   * 2.0 0.0 4.0
   * 0.0 2.0 4.0
   *
   * @param fileName nazwa pliku z wejściem
   * @return rozmiar macierzy, rozszerzona macierz
   */
  def readMatrixFromFile(fileName: String): (Int, Matrix) = {

    val source = Source.fromFile(fileName)
    val lines = source.getLines.toList
    val size = lines.head.trim.toInt
    val matrixLines = lines.slice(1, size + 1)
    val responseVector = lines.last.split(" ").map(_.trim.toDouble)

    val matrixRows = for {
      line <- matrixLines
    } yield Row(line.split(" ").map(_.trim.toDouble): _*)

    val rows =
      matrixRows.zip(responseVector).map { case (r, e) => r.appended(e) }

    (size, Matrix(rows: _*))

  }

  /**
   * Zapisuje do pliku macierz identyczności oraz wektor rozwiązań równania.
   * Dla wektora rozwiązań
   * 2.0 2.0
   * w pliku zostanie zapisane
   * 2
   * 1.0 0.0
   * 0.0 1.0
   * 2.0 2.0
   * @param resultVector wektor z rozwiazaniem układu równań
   * @param fileName nazwa pliku wyjściowego
   */
  def writeResultToFile(resultVector: Row, fileName: String) = {
    val size = resultVector.length
    val identityMatrix = Array.ofDim[Double](size, size)

    for(i <- 0 until size) identityMatrix(i)(i) = 1

    val file = new File(fileName)
    val bw = new BufferedWriter(new FileWriter(file))

    val matrixString = identityMatrix.map(_.mkString(" ")).mkString("\n")
    val resultString = resultVector.mkString(" ")
    bw.write(s"$size\n" ++ matrixString ++ "\n" ++ resultString)
    bw.close()
  }

  /**
   * Oblicza rozwiązania układu na podstawie macierzy trójkątnej górnej
   * @param matrix rozszerzona macierz w postaci trójkątnej górnej
   * @param size rozmiar układu równań
   * @return rozwiązania układu równań
   */
  def backwardSubstitution(matrix: Matrix, size: Int): Row = {
    val u = matrix.map(_.take(size)) // odrzucenie kolumny z wektorem Y
    val y = matrix.map(_.last) // wyodrębnienie wektora Y
    val x = Array.fill[Double](size)(0)

    for (i <- size - 1 to 0 by -1) {
      x(i) = y(i)
      for (j <- (i + 1) until size) {
        x(i) = x(i) - u(i)(j) * x(j)
      }
      x(i) = x(i) / u(i)(i)
    }

    x
  }

}
