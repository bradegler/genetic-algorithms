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

object ApplicationMain extends App {

    val scenario = Scenario(2048, 16384, 0.8f, 0.1f, 0.03f)

    val target = "my milkshake brings all the boys to the yard"
    val maxIterations = 500
    val populationCount = 1000
    val mutationRate = 0.01f;
    val geneCount = target.size

    val sim = Simulation(
        populationCount,
        mutationRate,
        maxIterations,
        geneCount,
        (i => (scala.util.Random.nextInt(96) + 32).toChar),
        linear(target),
        intelligentDesign(target))

    implicit val system = ActorSystem("simulation-system")
    implicit val timeout = Timeout(300.seconds)

    val actor = system.actorOf(SimulationFSM.props, "simulation-actor")
    actor ! SimulationStart(sim)
    system.actorOf(Props(classOf[Terminator], actor), "terminator")
}