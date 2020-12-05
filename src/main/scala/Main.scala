import Parallelization.{A, B, C, FoataClass, Scheduler}
import Parsing.MatrixParser

object Main extends App {
  require(args.length == 2, "Enter file name with matrix and file for output")


  val (size, matrix) = MatrixParser.readMatrixFromFile(args.head)

  //utowrzenie klas Foaty dla zadań A
  val foataA = for {
    i <- 0 until size - 1
  }
    yield new FoataClass((for (k <- i + 1 until size) yield A(matrix, i, k)).toList)


  /**
   * Metoda pomocnicza, do utworzenia zadań B bezpośrednio zależnych od danego zadania A
   *
   * @param taskA
   * @return lista zadań typu B bezpośrednio zależnych od podanego jako argument zadania A
   */
  def getChildrenTasksA(taskA: A): List[B] = {
    (for (j <- taskA.i to size) yield B(matrix, taskA.i, j, taskA.k, taskA)).toList
  }


  //obliczenie wszystkich klas Foaty zgodnie z obesrwacją, kolejne klasy zawierają zadania tylko jednego typu
  //oraz występują w powtarzającej się kolejności [A][B][C][A][B]....
  val allClasses = foataA.flatMap {
    aClass =>

      val b = aClass.tasks.flatMap(t => getChildrenTasksA(t.asInstanceOf[A]))//dla każdego zadania A oblicza zadania B
      val c = b.map(b => C(matrix, b.i, b.j, b.k, b))//dla każdego zadania B utworzenie odpowiadającego zadania C

      List(aClass, new FoataClass(b), new FoataClass(c)) //wstawienie w odpowiedniej kolejności
  }

  allClasses.foreach(c => Scheduler.computeTasksInParallel(c))//dla każdej z klas Foaty wykonanie równoległych obliczeń

  val result = MatrixParser.backwardSubstitution(matrix, size)//rozwiązanie układu równań poprzez podstawienie wstecz

  MatrixParser.writeResultToFile(result, args(1))//zapis rozwiązania do pliku

}