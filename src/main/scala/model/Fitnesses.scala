package model

object Fitnesses {

    /**
     * Rank the two sequences by the number of matches
     */
    def rank[A](source: Seq[A], target: Seq[A]): Float = {
        source.zip(target).foldLeft(0f)((acc, t) => if(t._1 == t._2) acc + 1 else acc)
    }

    /**
     * Calculates fitness as an equation
     * where each match of a gene to the target
     * produces a linear increase in fitness score.
     */
    def linear[A](target: Seq[A])(dna: DNA[A]): Float = {
        rank(dna.genes, target) / target.length
    }

    /**
     * Calculates fitness as an equation
     * where each match of a gene to the target
     * produces an exponential increase in fitness score.
     */
    def exponential[A](target: Seq[A])(dna: DNA[A]): Float = {
        val score = rank(dna.genes, target)
        (score * score) / (target.length * target.length)
    }

}