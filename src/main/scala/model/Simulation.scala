package model

case class GenerationResult[A](generation: Int, avgFitness: Double, bestFitness: Double, bestDNA: Option[DNA[A]])

class Simulation[A](
    populationSize: Int,
    mutationRate: Float,
    generations: Int,
    geneCount: Int,
    geneMutator: (Int => A),
    fitness: (DNA[A] => Float),
    crossover: ((DNA[A], DNA[A]) => DNA[A])) {

    def run: Unit = {
        val population = Population.initial(
            populationSize,
            mutationRate,
            geneCount,
            geneMutator,
            fitness,
            crossover)

        0.until(generations).foldLeft(population)((pop, i) => {
            if (pop.dnaList.size > 1) {
                val res = evaluate(pop, i)
                println(res)
                res.bestFitness match {
                    case 1.0 => Population.winner(pop, res.bestDNA.get)
                    case _ => pop.evolve
                }
            } else {
                pop
            }
        })
    }

    def evaluate(pop: Population[A], generation: Int): GenerationResult[A] = {
        val averageFitness = pop.dnaList.foldLeft(0.0)((res, p) => res + pop.fitness(p)) / pop.meta.size
        val best = pop.dnaList.foldLeft((0.0, None: Option[DNA[A]]))((res, p) => {
            val fit = pop.fitness(p)
            if (fit > res._1) {
                (fit, Some(p))
            } else {
                res
            }
        })
        GenerationResult(generation, averageFitness, best._1, best._2)
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
        crossover: ((DNA[A], DNA[A]) => DNA[A])): Simulation[A] = {
        new Simulation(populationSize, mutationRate, generations, geneCount, geneMutator, fitness, crossover)
    }
}
