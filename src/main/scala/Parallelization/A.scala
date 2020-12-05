package Parallelization

import Parsing.MatrixParser.Matrix


/**
 * Klasa dla zadania znalezienia dzielników dwóch wierszy
 * @param matrix referancja do macierzy
 * @param i indeks i-tego wiersza
 * @param k indeks k-tego wiersza
 * @param result zmienna zawierająca dzielnik dwóch wierszy, jeśli zadanie zostało już wykonane
 */
class A(matrix: Matrix, val i: Int, val k: Int, var result: Option[Double] = None) extends Task {

  override def compute(): Unit = {
    result = Some(matrix(k)(i) / matrix(i)(i))
  }

  override def hashCode(): Int = i * 293 + k * 31

  private def canEqual(a: Any): Boolean = a.isInstanceOf[A]

  override def equals(that: Any): Boolean =
    that match {
      case that: A => {
        that.canEqual(this) &&
          this.i == that.i &&
          this.k == that.k
      }
      case _ => false
    }
}

object A{
  def apply(matrix: Matrix, i: Int, k: Int): A = new A(matrix, i, k)
}
