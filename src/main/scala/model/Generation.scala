package model

case class Generation[A](id: Int, pop: Population[A])
case class GenerationResult[A](generation: Int, avgFitness: Double, bestFitness: Double, bestDNA: Option[DNA[A]])

object Generation {
    def evaluate[A](generation: Generation[A]): GenerationResult[A] = {
        val pop = generation.pop
        val averageFitness = pop.dnaList.foldLeft(0.0)((res, p) => res + pop.fitness(p)) / pop.meta.size
        val best = pop.dnaList.foldLeft((0.0, None: Option[DNA[A]]))((res, p) => {
            val fit = pop.fitness(p)
            if (fit > res._1) {
                (fit, Some(p))
            } else {
                res
            }
        })
        GenerationResult(generation.id, averageFitness, best._1, best._2)
    }
}