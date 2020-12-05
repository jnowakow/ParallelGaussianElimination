package Parallelization

import Parsing.MatrixParser.Matrix


/**
 * Klasa dla zadania pomnożenia elementu w wierszu i przez mnożnik wyznaczony w zadaniu A
 * @param matrix referancja do macierzy
 * @param i indeks i-tego wiersza
 * @param j indeks kolumny w wierszu i
 * @param k indeks k-tego wiersza
 * @param taskA zadanie A od którego bezpośrednio zależy ten task
 * @param result zmienna wartość matrix(i)(j) pomnożoną przez dzielnik dwóch wierszy
 */
class B(matrix: Matrix, val i: Int, val j: Int, val k: Int, taskA: A, var result: Option[Double] = None) extends Task {
  override def compute(): Unit = {
    taskA.result match {
      case None => sys.error("Scheduler doesn't work properly")
      case Some(v) => result = Some(matrix(i)(j) * v)
    }
  }

  override def hashCode(): Int = i * 293 + k * 31 + j * 17

  private def canEqual(a: Any): Boolean = a.isInstanceOf[B]

  override def equals(that: Any): Boolean =
    that match {
      case that: B => {
        that.canEqual(this) &&
          this.i == that.i &&
          this.k == that.k &&
          this.j == that.j
      }
      case _ => false
    }

}

object B{
  def apply(matrix: Matrix, i: Int, j: Int, k: Int, taskA: A): B = new B(matrix, i, j, k, taskA)
}