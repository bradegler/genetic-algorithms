package model

import scala.language.postfixOps

case class PopulationMeta(size: Int, mutationRate: Float)

class Population[A](
  val meta: PopulationMeta, 
  val geneCount: Int, 
  val geneMutator: (Int => A),
  val fitness: (DNA[A] => Float), 
  val crossover: ((DNA[A], DNA[A]) => DNA[A]), 
  val dnaList: List[DNA[A]]) {

  def evolve: Population[A] = {
    mate mutate
  }

  def evaluate(generation: Int): Option[DNA[A]] = {
    println(s"Generation $generation")
    val averageFitness = dnaList.foldLeft(0.0)((res, p) => res + fitness(p)) / meta.size
    val best = dnaList.foldLeft((0.0, None: Option[DNA[A]]))((res, p) => {
      val fit = fitness(p)
      if(fit > res._1) {
        (fit, Some(p))
      } else {
        res
      }
      })

    val bestFitness = best._1
    val bestDNA = best._2
    println(s"Average fitness $averageFitness")
    println(s"Best fitness $bestFitness")
    println(s"Best dna $bestDNA")
    if(bestFitness == 1.0) {
      bestDNA
    } else {
      None
    }
  }

  def mate: Population[A] = {
    val rand = scala.util.Random  
    val pool = dnaList.map(p => 0.until((fitness(p) * 100).toInt).map(i => p)).flatten
    val newDNA = 0.until(meta.size).map(i => crossover(pool(rand.nextInt(pool.size)), pool(rand.nextInt(pool.size)))).toList
    new Population[A](meta, geneCount, geneMutator, fitness, crossover, newDNA)
  }

  def mutate: Population[A] = {
    val rand = scala.util.Random  

    def m(a: A): A = {
      if(rand.nextFloat() < meta.mutationRate) {
        geneMutator(0)
      } else {
        a
      }
    }
    val newDNA = 0.until(meta.size).map(p => DNA.initial(geneCount, i => m(dnaList(p).genes(i)))).toList
    new Population[A](meta, geneCount, geneMutator, fitness, crossover, newDNA)
  }
}

object Population {
  def initial[A](size: Int, mutationRate: Float, geneCount: Int, geneMutator: (Int => A), fitness: (DNA[A] => Float), crossover: ((DNA[A], DNA[A]) => DNA[A])) = {
    val pop = 0.until(size).map(p => DNA.initial(geneCount, i => geneMutator(i))).toList
    new Population[A](PopulationMeta(size, mutationRate), geneCount, geneMutator, fitness, crossover, pop)
  }
  def winner[A](pop: Population[A], dna: DNA[A]) = {
    new Population[A](pop.meta, pop.geneCount, pop.geneMutator, pop.fitness, pop.crossover, List(dna))
  }
}
