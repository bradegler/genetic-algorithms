package model

case class DNA[A](genes: List[A])

object DNA {
  def initial[A](geneCount: Int, initializer: (Int => A)): DNA[A] = {
    new DNA(0.until(geneCount).map(initializer).toList)
  }
}
