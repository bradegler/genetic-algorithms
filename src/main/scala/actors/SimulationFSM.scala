package actors

import protocol._

import model._

import akka.actor._

sealed trait SimulationState
case object Init extends SimulationState
case object Running extends SimulationState
case object Complete extends SimulationState

sealed trait SimulationMeta
case object Uninitialized extends SimulationMeta
case class SimulationData(simulation: Simulation[Any]) extends SimulationMeta

object SimulationMeta {
    def empty: SimulationMeta = Uninitialized
    def withSim(sim: Simulation[Any]): SimulationMeta = SimulationData(sim)
}

class SimulationFSM extends Actor with LoggingFSM[SimulationState, SimulationMeta] with SimulationBehaviors {

    startWith(Init, SimulationMeta.empty)
    when(Init)(initBehavior)
    when(Running)(runBehavior)
    when(Complete)(FSM.NullFunction)

    onTransition {
        case _ -> Running => self ! SimulationRun
        case _ -> Complete => context stop self
    }

    initialize()
}

object SimulationFSM {
    def props: Props = Props(new SimulationFSM)
}

trait SimulationBehaviors {
    this: SimulationFSM =>

    val initBehavior: StateFunction = {
        case Event(SimulationStart(sim), m: SimulationMeta) => {
            goto(Running) using SimulationMeta.withSim(sim)
        }
    }

    val runBehavior: StateFunction = {
        case Event(SimulationRun, m: SimulationData) => {
            m.simulation.run
            self ! SimulationComplete
            stay()
        }
        case Event(SimulationComplete, m: SimulationMeta) =>
            goto(Complete) using(m)
    }
}