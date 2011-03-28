package sweeney.netimpl.genetic
package simple

import sweeney.ga._
import sweeney.nn._
import sweeney.nn.neuron._
import sweeney.nn.calc.combination._

abstract class GAPerceptron[T](val inputKeys:Seq[T], val outputKeys:Seq[T], val hiddenLayers:Seq[Int]) 
		extends GANeuralNetwork[IndexedSeq[Double],T] 
		with SimpleNeuralNetwork[T] with Testable[T]{
	
	override def determinePopulationFitness(currentPopulation:Seq[GeneticCode[IndexedSeq[Double]]]):Seq[CodeFitness[IndexedSeq[Double]]] = {
		for(gc <- currentPopulation) yield {
			setupNetworkForIndividual(gc.expressedIndividual)
			
			var error = 0.0
			for(data <- getTestData){
				val result = calculate(data._1)
				error += result.map{keyVal =>
					val outputError = data._2(keyVal._1) - keyVal._2
					outputError * outputError
				}.reduceLeft(_+_)
			}
			
			val fitness = if(error <= 0.0) 1000000.0 else 100.0 / error
			
			CodeFitness(gc,fitness)
		}
	}

}

