package Parallelization

object Scheduler {

  /**
   * Metoda pozwalająca wykonać zadania równolegle
   * @param foataClass klasa Foaty, której zadania mają zostać wykonane
   */
  def computeTasksInParallel(foataClass: FoataClass): Unit = {
    val threads = foataClass.tasks.map(t => new Thread {
      override def run(): Unit = {
        t.compute()
      }
    })

    threads.foreach(_.run())
    threads.foreach(_.join())
  }
}
