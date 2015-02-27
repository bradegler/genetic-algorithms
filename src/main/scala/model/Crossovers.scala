package model

object Crossovers {

  def split[A](dna1: DNA[A], dna2: DNA[A])(splitPoint: Int): DNA[A] =
    DNA(dna1.genes.splitAt(splitPoint)._1 ++ dna2.genes.splitAt(splitPoint)._2)

  def flip[A](dna1: DNA[A], dna2: DNA[A])(implicit f: Int => Boolean): DNA[A] = 
    DNA(0.until(dna1.genes.size).map(i => if(f(i)) dna1.genes(i) else dna2.genes(i)).toList)

  def randBool: Boolean = scala.util.Random.nextInt(1) == 0

  def rand[A](geneA: A, geneB: A): A = if(randBool) geneA else geneB

  def best[A](geneA: A, geneB: A)(target: A): A = {
    if(geneA == geneB || geneA == target) geneA 
    else if(geneB == target) geneB 
    else rand(geneA, geneB)
  }

  def intelligentDesign[A](target: Seq[A])(dna1: DNA[A], dna2: DNA[A]): DNA[A] = {
    DNA(0.until(dna1.genes.size).map(i => best(dna1.genes(i), dna2.genes(i))(target(i))).toList)
  }

  def randomSplit[A](dna1: DNA[A], dna2: DNA[A]): DNA[A] =
    split(dna1, dna2)(scala.util.Random.nextInt(dna1.genes.size))

  def randomFlip[A](dna1: DNA[A], dna2: DNA[A]): DNA[A] = {
    flip(dna1, dna2)(i => randBool)
  }


}
