import org.specs2._

import model.Crossovers
import model.DNA

class CrossoversSpec extends Specification { def is = s2"""

This is a specificiation to check the different crossover functions.


The split function should
    create a new DNA object by selecting a point and taking genes from left of the split point from object A and from the right from object B              $ms1
    
The flip function should
    create a new DNA object by taking the gene from either object A or object B based on the index of the gene                                             $cf1    


                                                                                                        """

    val dnaA = DNA.initial(4, i => i)        // Genes 0 1 2 3
    val dnaB = DNA.initial(4, i => i+4)      // Genes 4 5 6 7
    def ms1 = Crossovers.split(dnaA, dnaB)(2).genes should containTheSameElementsAs(List(0, 1, 6, 7))

    def cf1 = Crossovers.flip(dnaA, dnaB)(_ % 2 == 0).genes should containTheSameElementsAs(List(0, 2, 5, 7))

}