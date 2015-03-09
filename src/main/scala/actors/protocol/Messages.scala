package actors.protocol

import model._

trait Messages {

    sealed trait SimulationMessage
    case class SimulationStart(sim: Simulation[Any]) extends SimulationMessage
    case object SimulationRun extends SimulationMessage
    case object SimulationComplete extends SimulationMessage

    case class Evolve[A](pop: Population[A])

}
