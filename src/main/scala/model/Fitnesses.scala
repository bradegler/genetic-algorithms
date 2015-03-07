package model

object Fitnesses {

    /**
     * Calculates fitness as an equation
     * where each match of a gene to the target
     * produces a linear increase in fitness score.
     */
    def linear[A](target: Seq[A])(dna: DNA[A]): Float = {
        var score: Float = 0
        var tgt = target
        for (g <- dna.genes) {
            if (g == tgt.head) {
                score = score + 1
            }
            tgt = tgt.tail
        }
        score / target.length
    }

    /**
     * Calculates fitness as an equation
     * where each match of a gene to the target
     * produces an exponential increase in fitness score.
     */
    def exponential[A](target: Seq[A])(dna: DNA[A]): Float = {
        var score: Float = 0
        var tgt = target
        for (g <- dna.genes) {
            if (g == tgt.head) {
                score = score + 1
            }
            tgt = tgt.tail
        }
        (score * score) / (target.length * target.length)
    }

}