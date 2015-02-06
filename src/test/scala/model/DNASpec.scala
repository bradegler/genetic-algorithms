import org.specs2._

import model.DNA

class DNASpec extends Specification { def is = s2"""

This is a specification to check the DNA case class companion object

The 'DNA' class initial method should
    create a DNA object with the correct number of genes                                                    $e1
    create a DNA object with the genes initialized based on the initializer function                        $e2
                                                                                                        """

	val dna = DNA.initial(4, i => i)
	def e1 = dna.genes should have size 4
	def e2 = dna.genes should containTheSameElementsAs(List(0, 1, 2, 3))

}