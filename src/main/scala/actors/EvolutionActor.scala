package actors

import protocol._

import model._

import akka.actor._

class EvolutionActor extends Actor with ActorLogging with EvolutionHandler
{
    def receive() = {
        case Evolve(pop) => handleMessage(pop)
        case _ =>
    }
}

object EvolutionActor {
    def props: Props = Props(new EvolutionActor)
}

trait EvolutionHandler {

    def handleMessage[A](pop: Population[A]): Population[A] = {
        pop
    }
}