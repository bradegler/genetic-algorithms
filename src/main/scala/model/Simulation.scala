package model


class Simulation[+A](
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
                val res = Generation.evaluate(Generation[A](i, pop))
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
