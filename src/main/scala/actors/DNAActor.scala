package actors

import protocol._

import model._

import akka.actor._

class DNAActor extends Actor with ActorLogging
{
    def receive() = {
        case CalculateFitness(dna, fitness) => sender ! Fitness(dna, fitness(dna))
        case Crossover(dna1, dna2, crossover) => sender ! Crossed(crossover(dna1, dna2))
    }
}

object DNAActor {
    def props: Props = Props(new DNAActor)
}