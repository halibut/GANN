package sweeney.nn.calc.combination

import scala.collection.Seq

/**
 * Combination function that takes 1 or more inputs. If any
 * inputs have a value >= 0.5, then 1 is returned, otherwise
 * (all inputs are < .5) 0 is returned
 * 
 * If there are no inputs, 0 is returned
 */
class ORCombination extends InputCombinationFunction {

	def combineInputs(inputs: Seq[Double]): Double = {
		if(inputs.size == 0)
			return 0
		
		if(inputs.exists( _ >= .5))
			1
		else
			0
	}

}

object ORCombination extends ORCombination