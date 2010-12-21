package sweeney.nn.calc.combination

import scala.collection.Seq

/**
 * Combination function that takes 1 or more inputs and finds the average.
 * If the average is < .5, 1 is returned otherwise 0 is returned
 * 
 * If there are no inputs, 0 is returned
 */
class NOTCombination extends InputCombinationFunction {

	def combineInputs(inputs: Seq[Double]): Double = {
		if(inputs.size == 0)
			return 0
			
		val avg = average(inputs)
		
		if(avg < .5) 1 else 0
	}

}

object NOTCombination extends NOTCombination