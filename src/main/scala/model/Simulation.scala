package model


case class Simulation[A](
    scenario: Scenario,
    pop: Population[A]
)