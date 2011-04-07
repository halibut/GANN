package sweeney.gann.example.xor.ga

import sweeney.ga._

import sweeney.netimpl.genetic.simple._
import sweeney.nn._

object XORGeneticAlgorithm2 {
	def main(args : Array[String]) : Unit = {
		val inputKeys = Seq("1","2");
		val outputKeys = Seq("out");
		val hiddenLayers = Seq(6,3)
		
		val xorTestData = IndexedSeq(
			(Map("1" -> 1.0, "2" -> 0.0),Map("out" -> 1.0)),
			(Map("1" -> 0.0, "2" -> 0.0),Map("out" -> 0.0)),
			(Map("1" -> 0.0, "2" -> 1.0),Map("out" -> 1.0)),
			(Map("1" -> 1.0, "2" -> 1.0),Map("out" -> 0.0)))
  
		val popSize = 1000
		val maxGen = 1000
			
 		val network = new GAPerceptron[WeightBiasGeneticCode,String](inputKeys,outputKeys,hiddenLayers) with StringKey{
 			
 			override val populationSize = popSize
 			
 			override def mutationRate:Double = { 0.01 + .25 * (math.min(1.0,getGeneration/1000.0)) }
 			override def mutationAmount:Double = {0.05 + 2.0 * (math.max(0.0,(500.0 - getGeneration)/500.0)) }
 			override def crossoverRate:Double = { 0.9 }
 			override def elitistPercentile = {0.01 + 0.25 * (math.max(0.0,(200.0 - getGeneration)/200.0)) }
 	 			
 			override def setupNetworkForIndividual(individual:WeightBiasGeneticCode){
 				setWeights(individual.weights.geneSeq)
 				setBiases(individual.biases.geneSeq)
 			}
 			
 			override def getTestData:Seq[(Map[String,Double],Map[String,Double])] = {
 				xorTestData
 			}
	
 			override def stopCondition():Boolean = {
 				val gen = getGeneration
 				val pop = getPopulation
 				println(gen+" -> "+pop.slice(0,5).map(_.fitness)+" -> "+pop.reverse.slice(0,5).map(_.fitness))
 				(gen >= maxGen || pop.head.fitness >= 1000000)
 			}
 		}
		
		//Chromosome is interlaced biases and weights, so initial population 
		//has to be the right length
		val initialPopGC = for(i <- 0 until popSize) yield {
			val wChrom = new ChromosomeDouble((0 until network.weightsLength).map(i => 41 * math.random - 20.0).toIndexedSeq)
			val bChrom = new ChromosomeDouble((0 until network.biasesLength).map(i => 3 * math.random - 1.0).toIndexedSeq)
			new WeightBiasGeneticCode(wChrom,bChrom)
		}
		val initialPop = initialPopGC.map(gc => new CodeFitness(gc,0.0))
		
		//Setup the genetic algorithm's initial population
		network.initPopulation(initialPop,0)
		
		//Train the network
		network.trainNetwork()
		
		//Print the result
 		println(network.toString)
 		for(data <- xorTestData){
			println(data._1.toString+" -> "+network.calculate(data._1)("out"))
		}
	}
}
