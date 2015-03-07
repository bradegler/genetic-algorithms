import org.specs2._

import model.Crossovers
import model.DNA

class CrossoversSpec extends Specification { def is = s2"""

This is a specificiation to check the different crossover functions.


The split function should
    create a new DNA object by selecting a point and taking genes from left of the split point from object A and from the right from object B               $ms1
    
The flip function should
    create a new DNA object by taking the gene from either object A or object B based on the index of the gene                                              $cf1    

The best function should
    choose one of the genes if both genes are equal                                                                                                         $best1
    choose the first gene if it matches the target                                                                                                          $best2
    choose the second gene if the first gene does not match and the second gene matches the target                                                          $best3
    choose a gene randomly if neither match the target                                                                                                      $best4

The intelligentDesign function should
    choose the genes that match from both dna strands                                                                                                       $id1
    choose a random genes if neither gene matches the target                                                                                                $id2

                                                                                                        """

    val dnaA = DNA.initial(4, i => i)        // Genes 0 1 2 3
    val dnaB = DNA.initial(4, i => i+4)      // Genes 4 5 6 7
    def ms1 = Crossovers.split(dnaA, dnaB)(2) === DNA(Seq(0, 1, 6, 7))

    def cf1 = Crossovers.flip(dnaA, dnaB)(_ % 2 == 0) === DNA(Seq(0, 5, 2, 7))

    def best1 = Crossovers.best("s", "s")("l") === "s"
    def best2 = Crossovers.best("s", "l")("s") === "s"
    def best3 = Crossovers.best("l", "s")("s") === "s"
    def best4 = {
        val b = Crossovers.best("l", "d")("s")
        b === "l" || b === "d"
    }
    def id1 = Crossovers.intelligentDesign("bowl")(DNA("rnwl"), DNA("boeq")) === DNA("bowl")
    def id2 = {
        val i = Crossovers.intelligentDesign("b")(DNA("r"), DNA("q")) 
        i === DNA("r") || i === DNA("q")
    }
}