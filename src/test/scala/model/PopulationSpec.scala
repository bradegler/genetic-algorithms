import org.specs2._

import model._

class PopulationSpec extends Specification {
    def is = s2"""

This is a specification to check the Population class

The 'Population' object initial method should
    create a Population object with the correct population size         $init1
    create a Population object with the correct mutationRate            $init2
    create a Population object with the correct geneCount               $init3
    create a Population object with the correct geneMutator             $init4
    create a Population object with a dna list of the correct size      $init5

The 'Population' object winner method should
    produce a Population object with a single dna in its dna list       $winner1



                                                                                                        """

    val seed = (x: Int) => x.toString
    val mut = (x: String) => x.toString
    val initial = Population.initial[String](1, 0.0f, 10, seed, mut, Fitnesses.linear(Seq("test")), Crossovers.randomFlip)
    def init1 = initial.meta.size === 1
    def init2 = initial.meta.mutationRate === 0.0f
    def init3 = initial.geneCount === 10
    def init4 = initial.geneMutator === mut
    def init5 = initial.dnaList.size === 1

    val win = Population.winner[String](initial, DNA(Seq("test")))
    def winner1 = win.dnaList.size === 1

}