package model

import scala.language.postfixOps

case class Population[A](
    val geneMutator: (A => A),
    val fitness: (DNA[A] => Float),
    val crossover: ((DNA[A], DNA[A]) => DNA[A]),
    val dnaList: List[DNA[A]]) {
}

/**
 * Compaion object for the Population class.
 * Simple contructor methods.
 */


object Population {
    def winner[A](pop: Population[A], dna: DNA[A]) = {
        Population.withDNA(pop, List(dna))
    }
    def withDNA[A](pop: Population[A], dnaList: List[DNA[A]]): Population[A] = {
        Population[A](pop.geneMutator, pop.fitness, pop.crossover, dnaList)
    }
}
