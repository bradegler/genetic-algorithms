package actors

import protocol._

import model._

import akka.actor._
import akka.routing._

sealed trait PopulationState
case object Evolving extends PopulationState
case object Evaluating extends PopulationState
case object Mating extends PopulationState
case object Mutating extends PopulationState
case object Evolved extends PopulationState
case object Done extends PopulationState

sealed trait PopulationMeta[A]
case class FitnessMeta[A](fitness: List[(DNA[A], Float)]) extends PopulationMeta[A]
case class DNAMeta[A](dna: List[DNA[A]], res: GenerationResult[A]) extends PopulationMeta[A]
case class PopMeta[A](pop: Population[A], res: GenerationResult[A]) extends PopulationMeta[A]

class PopulationFSM[A](val scenario: Scenario, val generation: Generation[A], val originator: ActorRef) extends Actor with LoggingFSM[PopulationState, PopulationMeta[A]] {

    val rand = scala.util.Random

    val dnaActor = context.actorOf(RoundRobinPool(5).props(DNAActor.props))

    val evolveBehavior: StateFunction = {
        case Event(BeginEvolution, m: PopulationMeta[A]) => {
            generation.pop.dnaList.foreach(dna => dnaActor ! CalculateFitness(dna, generation.pop.fitness))
            goto(Evaluating)
        }
    }

    val evalBehavior: StateFunction = {
        case Event(f: Fitness[A], m: FitnessMeta[A]) => {
            val meta = FitnessMeta((f.dna, f.fit) +: m.fitness)
            if(meta.fitness.size == scenario.populationSize)
                goto(Mating) using (meta)
            else
                stay using (meta)
        }
    }

    val matingBehavior: StateFunction = {
        case Event(Mate, m: FitnessMeta[A]) => {
            val pool = m.fitness.map { case (dna, fit) => 0.until((fit * 100).toInt).map(i => dna) }.flatten
            0.until(scenario.populationSize)
                .foreach(_ => dnaActor ! Crossover(pool(rand.nextInt(pool.size)), pool(rand.nextInt(pool.size)), generation.pop.crossover))
            stay using (DNAMeta(List(), evaluate(m)))
        }
        case Event(c: Crossed[A], m: DNAMeta[A]) => {
            val meta = DNAMeta(c.dna +: m.dna, m.res)
            if(meta.dna.size == scenario.populationSize)
                goto(Mutating) using (meta)
            else
                stay using (meta)
        }
    }

    val mutatingBehavior: StateFunction = {
        case Event(Mutate, m: DNAMeta[A]) => {
            def mute(a: A): A = if (rand.nextFloat() < scenario.mutationRatio) generation.pop.geneMutator(a) else a
            val newDNA = 0.until(scenario.populationSize).map(p => DNA.initial(scenario.dnaSize, i => mute(m.dna(p).genes(i)))).toList
            goto(Evolved) using (PopMeta(Population.withDNA(generation.pop, newDNA), m.res))
        }
    }

    val evolvedBehavior: StateFunction = {
        case Event(Report, m: PopMeta[A]) => {
            context.parent ! CompletedEvolution(Generation(generation.id, m.pop), m.res, originator)
            goto(Done)
        }
    }

    def evaluate[A](m: FitnessMeta[A]): GenerationResult[A] = {
        val averageFitness = m.fitness.foldLeft(0.0)((res, p) => res + p._2 / scenario.populationSize)
        val best = m.fitness.foldLeft((0.0, None: Option[DNA[A]]))((res, p) => {
            val fit = p._2
            if (fit > res._1) {
                (fit, Some(p._1))
            } else {
                res
            }
        })
        GenerationResult(generation.id, averageFitness, best._1, best._2)
    }

    def inRange[A](a: A, rate: Float, f: A => A): A = if (rand.nextFloat() < rate) f(a) else a

    startWith(Evolving, FitnessMeta(List()))
    when(Evolving)(evolveBehavior)
    when(Evaluating)(evalBehavior)
    when(Mating)(matingBehavior)
    when(Mutating)(mutatingBehavior)
    when(Evolved)(evolvedBehavior)
    when(Done)(FSM.NullFunction)

    onTransition {
        case _ -> Mating => self ! Mate
        case _ -> Mutating => self ! Mutate
        case _ -> Evolved => self ! Report
        case _ -> Done => context stop self
    }


    initialize()


}

object PopulationFSM {
    def props[A](scenario: Scenario, gen: Generation[A], originator: ActorRef): Props = Props(new PopulationFSM(scenario, gen, originator))
}