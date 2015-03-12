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
        // Mate
        val rand = scala.util.Random
        // Build the mating pool
        val pool = pop.dnaList.map(p => 0.until((pop.fitness(p) * 100).toInt).map(i => p)).flatten
        // Crossover
        val crossedDNA = 0.until(pop.meta.size).map(i => pop.crossover(pool(rand.nextInt(pool.size)), pool(rand.nextInt(pool.size)))).toList
        // Mutation function
        def m(a: A): A = if (rand.nextFloat() < pop.meta.mutationRate) pop.geneMutator(0) else a
        // Mutate
        val newDNA = 0.until(pop.meta.size).map(p => DNA.initial(pop.geneCount, i => m(crossedDNA(p).genes(i)))).toList
        Population.withDNA(pop, newDNA)
    }
}