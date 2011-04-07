package sweeney.netimpl.genetic
package simple

import sweeney.ga._
import sweeney.nn._
import sweeney.nn.neuron._
import sweeney.nn.calc.combination._

abstract class GAPerceptron[I,NK](val inputKeys:Seq[NK], val outputKeys:Seq[NK], val hiddenLayers:Seq[Int]) 
		extends GANeuralNetwork[I,NK] 
		with SimpleNeuralNetwork[NK] with Testable[NK]{
	
	override def determinePopulationFitness(currentPopulation:Seq[GeneticCode[I]]):Seq[CodeFitness[I]] = {
		for(gc <- currentPopulation) yield {
			setupNetworkForIndividual(gc.expressedIndividual)
			
			val errorSeq = for(data <- getTestData) yield {
				val result = calculate(data._1)
				result.map{keyVal =>
					val expected = data._2(keyVal._1)
					val actual = keyVal._2
					val outputError = expected - actual 
					math.abs(outputError) // * outputError
				}.reduceLeft(_+_)
			}
			
			val error = errorSeq.reduceLeft(_+_)
			
			val fitness = 
				if(error == 0.0)
					Double.MaxValue
				else 
					1.0 / error
			
			CodeFitness(gc,fitness)
		}
	}

}

