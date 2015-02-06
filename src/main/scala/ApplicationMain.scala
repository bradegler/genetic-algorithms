import model._
import model.Crossovers._
import model.Fitnesses._

object ApplicationMain extends App {
  val target = "my milkshake brings all the boys to the yard"
  val maxIterations = 500
  val populationCount = 1000
  val mutationRate = 0.01f;
  val geneCount = target.size

  Simulation(
  	populationCount, 
  	mutationRate, 
  	maxIterations, 
  	geneCount, 
  	(i => (scala.util.Random.nextInt(96) + 32).toChar), 
  	linear(target), 
  	intelligentDesign(target)
  ).run
}