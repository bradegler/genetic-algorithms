package actors

import protocol._

import model._

import akka.actor._

class EvolutionActor(val scenario: Scenario) extends Actor with ActorLogging with EvolutionHandler
{
    def receive() = {
        case StartEvolution(pop) => self ! Evolve(0, pop, sender)
        case Evolve(generation, pop, sender) if generation > scenario.maxGenerations => sender ! SimulationComplete(pop)
        case Evolve(generation, pop, sender) => context.actorOf(PopulationFSM.props(scenario, Generation(generation, pop), sender)) ! BeginEvolution
        case CompletedEvolution(gen, res, sender) => handle(gen, res, sender)
    }
}

object EvolutionActor {
    def props(scenario: Scenario): Props = Props(new EvolutionActor(scenario))
}

trait EvolutionHandler {
    this: EvolutionActor =>

    def handle[A](gen: Generation[A], res: GenerationResult[A], sender: ActorRef): Unit = {
        println(res)
        res.bestFitness match {
            case 1.0 => sender ! SimulationComplete(Population.winner(gen.pop, res.bestDNA.get))
            case _ => self ! Evolve(gen.id + 1, gen.pop, sender)
        }
    }
}