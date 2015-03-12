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

    def mate: Population[A] = {
        val rand = scala.util.Random
        val pool = dnaList.map(p => 0.until((fitness(p) * 100).toInt).map(i => p)).flatten
        val newDNA = 0.until(meta.size).map(i => crossover(pool(rand.nextInt(pool.size)), pool(rand.nextInt(pool.size)))).toList
        new Population[A](meta, geneCount, geneMutator, fitness, crossover, newDNA)
    }

    def mutate: Population[A] = {
        def m(a: A): A = if (scala.util.Random.nextFloat() < meta.mutationRate) geneMutator(0) else a
        val newDNA = 0.until(meta.size).map(p => DNA.initial(geneCount, i => m(dnaList(p).genes(i)))).toList
        new Population[A](meta, geneCount, geneMutator, fitness, crossover, newDNA)
    }
}

/**
 * Compaion object for the Population class.
 * Simple contructor methods.
 */
object Population {
    def initial[A](size: Int, mutationRate: Float, geneCount: Int, geneMutator: (Int => A), fitness: (DNA[A] => Float), crossover: ((DNA[A], DNA[A]) => DNA[A])) = {
        val pop = 0.until(size).map(p => DNA.initial(geneCount, i => geneMutator(i))).toList
        new Population[A](PopulationMeta(size, mutationRate), geneCount, geneMutator, fitness, crossover, pop)
    }
    def winner[A](pop: Population[A], dna: DNA[A]) = {
        Population.withDNA(pop, List(dna))
    }
    def withDNA[A](pop: Population[A], dnaList: List[DNA[A]]): Population[A] = {
        new Population[A](pop.meta, pop.geneCount, pop.geneMutator, pop.fitness, pop.crossover, dnaList)
    }
}
