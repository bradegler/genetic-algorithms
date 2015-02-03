package com.barfight

class Simulation[A](
  populationSize: Int,
  mutationRate: Float,
  generations: Int,
  geneCount: Int,
  geneMutator: (Int => A),
  fitness: (DNA[A] => Float),
  crossover: ((DNA[A], DNA[A]) => DNA[A])
  ) {

  def run: Unit = {
    val population = Population.initial(
      populationSize,
      mutationRate, 
      geneCount, 
      geneMutator, 
      fitness, 
      crossover)
 
    0.until(generations).foldLeft(population)((pop, i) => {
        if(pop.dnaList.size > 1) {
          pop.evaluate(i) match {
            case Some(winner) => 
              println(s"Winning dna discovered in generation $i - $winner")
              Population.winner(pop, winner)
            case None => pop.evolve
          }
        } else {
          pop
        }
      })
  }

}

object Simulation {
  def apply[A](
    populationSize: Int,
    mutationRate: Float,
    generations: Int,
    geneCount: Int,
    geneMutator: (Int => A),
    fitness: (DNA[A] => Float),
    crossover: ((DNA[A], DNA[A]) => DNA[A])
    ): Simulation[A] = {
    new Simulation(populationSize, mutationRate, generations, geneCount, geneMutator, fitness, crossover)    
  }
}

case class PopulationMeta(size: Int, mutationRate: Float)

class Population[A](
  val meta: PopulationMeta, 
  val geneCount: Int, 
  val geneMutator: (Int => A),
  val fitness: (DNA[A] => Float), 
  val crossover: ((DNA[A], DNA[A]) => DNA[A]), 
  val dnaList: List[DNA[A]]) {

  def evolve: Population[A] = {
    mate mutate
  }

  def evaluate(generation: Int): Option[DNA[A]] = {
    println(s"Generation $generation")
    val averageFitness = dnaList.foldLeft(0.0)((res, p) => res + fitness(p)) / meta.size
    val best = dnaList.foldLeft((0.0, None: Option[DNA[A]]))((res, p) => {
      val fit = fitness(p)
      if(fit > res._1) {
        (fit, Some(p))
      } else {
        res
      }
      })

    val bestFitness = best._1
    val bestDNA = best._2
    println(s"Average fitness $averageFitness")
    println(s"Best fitness $bestFitness")
    println(s"Best dna $bestDNA")
    if(bestFitness == 1.0) {
      bestDNA
    } else {
      None
    }
  }

  def mate: Population[A] = {
    val rand = scala.util.Random  
    val pool = dnaList.map(p => 0.until((fitness(p) * 100).toInt).map(i => p)).flatten
    val newDNA = 0.until(meta.size).map(i => crossover(pool(rand.nextInt(pool.size)), pool(rand.nextInt(pool.size)))).toList
    new Population[A](meta, geneCount, geneMutator, fitness, crossover, newDNA)
  }

  def mutate: Population[A] = {
    val rand = scala.util.Random  

    def m(a: A): A = {
      if(rand.nextFloat() < meta.mutationRate) {
        geneMutator(0)
      } else {
        a
      }
    }
    val newDNA = 0.until(meta.size).map(p => DNA.initial(geneCount, i => m(dnaList(p).genes(i)))).toList
    new Population[A](meta, geneCount, geneMutator, fitness, crossover, newDNA)
  }
}

object Population {
  def initial[A](size: Int, mutationRate: Float, geneCount: Int, geneMutator: (Int => A), fitness: (DNA[A] => Float), crossover: ((DNA[A], DNA[A]) => DNA[A])) = {
    val pop = 0.until(size).map(p => DNA.initial(geneCount, i => geneMutator(i))).toList
    new Population[A](PopulationMeta(size, mutationRate), geneCount, geneMutator, fitness, crossover, pop)
  }
  def winner[A](pop: Population[A], dna: DNA[A]) = {
    new Population[A](pop.meta, pop.geneCount, pop.geneMutator, pop.fitness, pop.crossover, List(dna))
  }
}

case class DNA[A](genes: List[A])

object DNA {
  def initial[A](geneCount: Int, initializer: (Int => A)): DNA[A] = {
    new DNA(0.until(geneCount).map(initializer).toList)
  }
}

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