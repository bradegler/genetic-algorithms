package actors.protocol

import model._

import akka.actor._

trait Messages {

    sealed trait SimulationMessage
    case class SimulationStart[A](sim: Simulation[A]) extends SimulationMessage
    case object SimulationRun extends SimulationMessage
    case class SimulationComplete[A](pop: Population[A]) extends SimulationMessage

    case class StartEvolution[A](pop: Population[A])
    case class Evolve[A](generation: Int, pop: Population[A], originator: ActorRef)

    case object BeginEvolution
    case object Mate
    case object Mutate
    case object Report
    case class Fitness[A](dna: DNA[A], fit: Float)
    case class Crossed[A](dna: DNA[A])
    case class CompletedEvolution[A](gen: Generation[A], res: GenerationResult[A], originator: ActorRef)

    case class CalculateFitness[A](dna: DNA[A], fitness: (DNA[A] => Float))
    case class Crossover[A](dna1: DNA[A], dna2: DNA[A], crossover: (DNA[A], DNA[A]) => DNA[A])


}
