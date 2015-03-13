import actors._
import actors.protocol._

import model._
import model.Crossovers._
import model.Fitnesses._

import scala.concurrent.Await
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._

class Terminator(ref: ActorRef) extends Actor with ActorLogging {
  context watch ref
  def receive = {
    case Terminated(_) =>
      log.info("{} has terminated, shutting down system", ref.path)
      context.system.shutdown()
  }
}

object Sim {
  def run {
    def rchar: Char = (scala.util.Random.nextInt(95) + 32).toChar
    def seedPopulation[A](size: Int, geneCount: Int, seed: Int => A): List[DNA[A]] = 0.until(size).map(p => DNA.initial(geneCount, i => seed(i))).toList

    implicit val system = ActorSystem("simulation-system")
    implicit val timeout = Timeout(300.seconds)

    val target = "my milkshake brings all the boys to the yard"
    val scenario = Scenario(target.size, 2048, 16384, 0.8f, 0.1f, 0.03f)

    val seedDNA = seedPopulation(scenario.populationSize, target.length, ((i: Int) => rchar))
    val pop = Population(((a: Char) => rchar), linear(target), randomSplit[Char], seedDNA)

    val sim = Simulation(scenario, pop)
    val actor = system.actorOf(SimulationFSM.props, "simulation-actor")
    actor ! SimulationStart(sim)
    system.actorOf(Props(classOf[Terminator], actor), "terminator")
  }
}

object ApplicationMain extends App {
  Sim.run
}