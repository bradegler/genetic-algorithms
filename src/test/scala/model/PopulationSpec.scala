import org.specs2._

import model._

class PopulationSpec extends Specification {
    def is = s2"""

This is a specification to check the Population class

The 'Population' object withDNA method should
    create a Population object copying the geneMutator function         $wdna1
    create a Population object copying the fitness function             $wdna2
    create a Population object copying the crossover function           $wdna3
    create a Population object with the new dna list                    $wdna4

The 'Population' object winner method should
    produce a Population object with a single dna in its dna list       $winner1



                                                                                                        """

    val initial = Population((x: String) => x.toString, Fitnesses.linear(Seq("test")), Crossovers.randomFlip[String], List())
    val ndna = List(DNA(Seq("copy")))
    val copy = Population.withDNA(initial, ndna)
    def wdna1 = copy.geneMutator === initial.geneMutator
    def wdna2 = copy.fitness === initial.fitness
    def wdna3 = copy.crossover === initial.crossover
    def wdna4 = copy.dnaList === ndna

    val win = Population.winner[String](initial, DNA(Seq("test")))
    def winner1 = win.dnaList.size === 1

}