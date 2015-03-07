package model

/**
 * Collection of crossover algorithms.
 * A crossover is what occurs when you merge two DNA objects into a
 * single new DNA object.
 */
object Crossovers {

    /**
     * Split the two dna objects at a certian split point, take the
     * genes from the first DNA that are from the left side of the split and
     * the genes from the second DNA thare are from the right side of the split
     */
    def split[A](dna1: DNA[A], dna2: DNA[A])(splitPoint: Int): DNA[A] =
        DNA(dna1.genes.splitAt(splitPoint)._1 ++ dna2.genes.splitAt(splitPoint)._2)

    /**
     * Genes are chosen based on the truth function. True values select the
     * gene from the left, false values select the gene from the right.
     */
    def flip[A](dna1: DNA[A], dna2: DNA[A])(implicit f: Int => Boolean): DNA[A] =
        DNA(0.until(dna1.genes.size).map(i => if (f(i)) dna1.genes(i) else dna2.genes(i)).toList)

    /**
     * Crossover function that selects all genes that match the target.
     * If neither gene matches, it selects one of the genes at random.
     */
    def best[A](geneA: A, geneB: A)(target: A): A = {
        if (geneA == geneB || geneA == target) geneA
        else if (geneB == target) geneB
        else rand(geneA, geneB)
    }

    def intelligentDesign[A](target: Seq[A])(dna1: DNA[A], dna2: DNA[A]): DNA[A] = {
        DNA(0.until(dna1.genes.size).map(i => best(dna1.genes(i), dna2.genes(i))(target(i))).toList)
    }

    def randBool: Boolean = scala.util.Random.nextInt(1) == 0

    def rand[A](geneA: A, geneB: A): A = if (randBool) geneA else geneB

    def randomSplit[A](dna1: DNA[A], dna2: DNA[A]): DNA[A] =
        split(dna1, dna2)(scala.util.Random.nextInt(dna1.genes.size))

    def randomFlip[A](dna1: DNA[A], dna2: DNA[A]): DNA[A] = {
        flip(dna1, dna2)(i => randBool)
    }

}
