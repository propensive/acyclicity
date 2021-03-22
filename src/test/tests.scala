package acyclicity.test

import acyclicity._

import probably._

object Tests extends Suite("Acyclicity tests") {

  def run(test: Runner): Unit = {
    test("Remove single node") {
      Dag.from(1 -> 2, 2 -> 3) - 2
    }.assert(_ == Dag.from(1 -> 3))
    
    test("Filter nodes") {
      val dag = Dag.from(1 -> 2, 2 -> 3)
      println(dag.edges)
      dag.filter(_ != 2)
    }.assert(_ == Dag(1 -> Set(3)))
    
    test("Filter first node") {
      Dag.from(1 -> 2, 2 -> 3).filter(_ != 1)
    }.assert(_ == Dag.from(2 -> 3))
    
    test("Keep node") {
      val dag = Dag.from(1 -> 2, 2 -> 3).filter(_ == 2)
      println(dag.dot)
      dag
    }.assert(_ == Dag(2 -> Set[Int]()))
    
    test("Transitively filter nodes") {
      Dag.from(1 -> 2, 2 -> 3, 3 -> 4).filter { n => n == 1 || n == 4 }
    }.assert(_ == Dag.from(1 -> 4))
  }
}
