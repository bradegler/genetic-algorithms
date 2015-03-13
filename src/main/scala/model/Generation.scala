package model

case class Generation[A](id: Int, pop: Population[A])
case class GenerationResult[A](generation: Int, avgFitness: Double, bestFitness: Double, bestDNA: Option[DNA[A]])