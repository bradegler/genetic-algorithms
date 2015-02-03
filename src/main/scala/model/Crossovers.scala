package model

object Crossovers {

  def randomSplit[A](dna1: DNA[A], dna2: DNA[A]): DNA[A] = {
    val rand = scala.util.Random  
    val splitPoint = rand.nextInt(1)
    val genes = dna1.genes.splitAt(splitPoint)._1 ++ dna2.genes.splitAt(splitPoint)._2
    DNA(genes)
  }

  def coinFlip[A](dna1: DNA[A], dna2: DNA[A]): DNA[A] = {
    val rand = scala.util.Random  
    val genes = 0.until(dna1.genes.size).map(i => {
        if(rand.nextInt(1) == 0) {
          dna1.genes(i)
        } else {
          dna2.genes(i)
        }
      }).toList
    DNA(genes)
  }

  def intelligentDesign[A](target: Seq[A])(dna1: DNA[A], dna2: DNA[A]): DNA[A] = {
    val rand = scala.util.Random  
    val genes = 0.until(dna1.genes.size).map(i => {
        val g1 = dna1.genes(i)
        val g2 = dna2.genes(i)
        val tv = target(i)
        if(g1 == g2) {
          g1
        } else if(g1 == tv) {
          g1
        } else if(g2 == tv) {
          g2
        } else {
          if(rand.nextInt(1) == 0) {
            g1
          } else {
            g2
          }
        }
      }).toList
    DNA(genes)
  }

}
