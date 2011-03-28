package sweeney.gann.example.xor.ga

import sweeney.ga._

import sweeney.netimpl.genetic.simple._
import sweeney.nn._

object XORGeneticAlgorithm {
	def main(args : Array[String]) : Unit = {
		val inputKeys = Seq("1","2");
		val outputKeys = Seq("out");
		val hiddenLayers = Seq(4,2)
		
		val xorTestData = IndexedSeq(
			(Map("1" -> 1.0, "2" -> 0.0),Map("out" -> 1.0)),
			(Map("1" -> 0.0, "2" -> 0.0),Map("out" -> 0.0)),
			(Map("1" -> 0.0, "2" -> 1.0),Map("out" -> 1.0)),
			(Map("1" -> 1.0, "2" -> 1.0),Map("out" -> 0.0)))
  
		val popSize = 1000
		val maxGen = 10000
			
 		val network = new GAPerceptron(inputKeys,outputKeys,hiddenLayers) with StringKey{
 			
 			override val populationSize = popSize
 			//override def elitePercentile = 0.03
 			//override def mutationRate = 0.1
 			
 			override def setupNetworkForIndividual(individual:IndexedSeq[Double]){
 				setInterleavedBiasesAndWeights(individual)
 			}
 			
 			override def getTestData:Seq[(Map[String,Double],Map[String,Double])] = {
 				xorTestData
 			}
	
 			override def stopCondition():Boolean = {
 				val gen = getGeneration
 				val pop = getPopulation
 				println(gen+" -> "+pop.slice(0,5).map(_.fitness)+" -> "+pop.reverse.slice(0,5).map(_.fitness))
 				(gen >= maxGen || pop.head.fitness >= 100000000)
 			}
 		}
		
		//Chromosome is interlaced biases and weights, so initial population 
		//has to be the right length
		val chromosomeLength = network.weightsLength + network.biasesLength
		val initialPop = for(i <- 0 until popSize) yield {
			val chrom = (0 until chromosomeLength).map(i => 3 * math.random -1).toIndexedSeq
			val code = new SimpleGeneticCode(chrom)
			val cf = CodeFitness(code,0.0)
			cf
		}
 		
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
