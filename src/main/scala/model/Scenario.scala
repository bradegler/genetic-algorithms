package model

case class Scenario(
    // The number of genes in a dna strand
    dnaSize: Int,
    // The size of the simulation population
    populationSize: Int,
    // The maximum number of generations for the simulation.
    maxGenerations: Int,
    // The probability of crossover for any member of the population,
    // where 0.0 <= crossoverRatio <= 1.0
    crossoverRatio: Float,
    // The portion of the population that will be retained without change
    // between evolutions, where 0.0 <= elitismRatio < 1.0
    elitismRatio: Float,
    // The probability of mutation for any member of the population,
    // where 0.0 <= mutationRatio <= 1.0
    mutationRatio: Float
)