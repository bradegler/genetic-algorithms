package model

object Fitnesses {
  def linear[A](target: Seq[A])(dna: DNA[A]): Float = {
    var score: Float = 0
    var tgt = target
    for(g <- dna.genes) {
      if(g == tgt.head) {
        score = score + 1
      }
      tgt = tgt.tail
    }
    score / target.length
  }

  def exponential[A](target: Seq[A])(dna: DNA[A]): Float = {
    var score: Float = 0
    var tgt = target
    for(g <- dna.genes) {
      if(g == tgt.head) {
        score = score + 1
      }
      tgt = tgt.tail
    }
    (score * score) / (target.length * target.length)
  }

}