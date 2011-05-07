package sweeney.gann.example.xor.ga

import sweeney.netimpl.genetic.testing.ErrorBasedTesting
import sweeney.ga._
import sweeney.netimpl.genetic.PagedGANN
import sweeney.netimpl.genetic.nettype.GAPerceptron
import sweeney.nn._

object XORGeneticAlgorithm2 {
  
  	def main(args : Array[String]) : Unit = {
		
		
		val xorTestData = IndexedSeq(
			(Map("1" -> 1.0, "2" -> 0.0),Map("out" -> 1.0)),
			(Map("1" -> 0.0, "2" -> 0.0),Map("out" -> 0.0)),
			(Map("1" -> 0.0, "2" -> 1.0),Map("out" -> 1.0)),
			(Map("1" -> 1.0, "2" -> 1.0),Map("out" -> 0.0)))
  
		val popSize = 1000
		val maxGen = 5000
			
		val gann = new PagedGANN[WeightBiasGeneticCode,String,Perceptron[String]]() 
						with ErrorBasedTesting[WeightBiasGeneticCode,String,Perceptron[String]]
						with GAPerceptron[WeightBiasGeneticCode,String]{
		  
			override def getTestData = xorTestData
		  
			override val inputKeys = Seq("1","2");
			override val outputKeys = Seq("out");
			override val hiddenLayers = Seq(6,3);
			
			override val populationSize = popSize
 			
 			override def mutationRate:Double = { 0.25 + .25 * (math.min(1.0,getGeneration/1000.0)) }
 			override def mutationSize:Double = {0.25 + 2.0 * (math.max(0.0,(500.0 - getGeneration)/500.0)) }
 			override def crossoverRate:Double = { 0.9 }
 			override def elitistPercentile = {0.01 + 0.25 * (math.max(0.0,(200.0 - getGeneration)/200.0)) }
 			override def minNeuronOutput:Double = -0.01
 			override def maxNeuronOutput:Double = 1.01
 			
 			override def concurrentPages = 4
			
 			override def setupNetworkForIndividual(network:Perceptron[String],individual:WeightBiasGeneticCode){
 				network.setWeights(individual.weights.geneSeq)
 				network.setBiases(individual.biases.geneSeq)
 			}
			
 			override def stopCondition():Boolean = {
 				val gen = getGeneration
 				val topFive = getPopulation(0,5)
 				val bottomFive = getPopulation(populationSize - 5)
 				println(gen+" -> "+topFive.map(_._2)+" -> "+bottomFive.reverse.map(_._2))
 				(gen >= maxGen || topFive.head._2 >= 1000000)
 			}
			
			override def generateHiddenNeuronKey(layer:Int,index:Int):String = {
				"Hidden-"+layer+"-"+index;
			}
		}
		

		//Genetic Code is 2 chromosomes (1 for weights, 1 for biases)
		val initialPop = for(i <- 0 until popSize) yield {
			val wChrom = new ChromosomeDouble((0 until gann.weightsLength).map(i => 20.0 * math.random - 10.0).toIndexedSeq)
			val bChrom = new ChromosomeDouble((0 until gann.biasesLength).map(i => 2.0 * math.random - 1.0).toIndexedSeq)
			(new WeightBiasGeneticCode(wChrom,bChrom),0.0)
		}
 		
		//Setup the genetic algorithm's initial population
		gann.initPopulation(initialPop,0)
		
		//Train the network
		val network = gann.trainNetwork()
		
		//Print the result
 		println(network.toString)
 		for(data <- xorTestData){
			println(data._1.toString+" -> "+network.calculate(data._1)("out"))
		}
	}
}
