package Parallelization

import Parsing.MatrixParser.Matrix


/**
 * Klasa dla zadania odjęcia elementu z wiersza i od elmentu z wiersza k
 * @param matrix referancja do macierzy
 * @param i indeks i-tego wiersza
 * @param j indeks kolumny w wierszu i
 * @param k indeks k-tego wiersza
 * @param taskB zadanie B od którego bezpośrednio zależy ten task
 */
class C(matrix: Matrix, val i: Int, val j: Int, val k: Int, taskB: B) extends Task {
  override def compute(): Unit = {
    taskB.result match {
      case None => sys.error("Scheduler doesn't work properly")
      case Some(v) => matrix(k)(j) -= v
    }
  }

  override def hashCode(): Int = i * 17 + k * 293 + j * 31

  private def canEqual(a: Any): Boolean = a.isInstanceOf[C]

  override def equals(that: Any): Boolean =
    that match {
      case that: C => {
        that.canEqual(this) &&
          this.i == that.i &&
          this.k == that.k &&
          this.j == that.j
      }
      case _ => false
    }
}

object C {
  def apply(matrix: Matrix, i: Int, j: Int, k: Int, taskB: B): C = new C(matrix, i, j, k, taskB)
}
