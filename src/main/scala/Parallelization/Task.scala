package Parallelization


/**
 * Interfejs dla definiowania niepodzielnych zadań obliczeniowych
 */
trait Task {

  //wykonanie obliczeń dla danego zadania
  def compute(): Unit
}
