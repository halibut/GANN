package sweeney.gann.example.xor.ga

import sweeney.netimpl.genetic.testing.ErrorBasedTesting
import sweeney.ga._

import sweeney.netimpl.genetic._
import sweeney.netimpl.genetic.nettype._
import sweeney.nn._

object XORGeneticAlgorithm {
	def main(args : Array[String]) : Unit = {
		
		
		val xorTestData = IndexedSeq(
			(Map("1" -> 1.0, "2" -> 0.0),Map("out" -> 1.0)),
			(Map("1" -> 0.0, "2" -> 0.0),Map("out" -> 0.0)),
			(Map("1" -> 0.0, "2" -> 1.0),Map("out" -> 1.0)),
			(Map("1" -> 1.0, "2" -> 1.0),Map("out" -> 0.0)))
  
		val popSize = 1000
		val maxGen = 10000
			
		val gann = new PagedGANN[IndexedSeq[Double],String, Perceptron[String]]() 
						with ErrorBasedTesting[IndexedSeq[Double],String,Perceptron[String]]
						with GAPerceptron[IndexedSeq[Double],String]{
		  
			override def getTestData = xorTestData
		  
			override val inputKeys = Seq("1","2");
			override val outputKeys = Seq("out");
			override val hiddenLayers = Seq(4,2);
			
			override val populationSize = popSize
 			override def mutationRate:Double = { 0.1 }
 			override def mutationSize:Double = { 0.5 }
 			override def crossoverRate:Double = { 0.9 }
 			
 			override def concurrentPages = 4
 			
 			override def setupNetworkForIndividual(network:Perceptron[String],individual:IndexedSeq[Double]){
 				network.setInterleavedBiasesAndWeights(individual)
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
		
		//Chromosome is interlaced biases and weights, so initial population 
		//has to be the right length
		val chromosomeLength = gann.weightsLength + gann.biasesLength
		val initialPop = for(i <- 0 until popSize) yield {
			val chrom = (0 until chromosomeLength).map(i => 5 * math.random - 2.0).toIndexedSeq
			val code = new ChromosomeDouble(chrom)
			val cf = (code,1.0)
			cf
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
