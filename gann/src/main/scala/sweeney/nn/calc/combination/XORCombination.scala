package sweeney.nn.calc.combination

import scala.collection.Seq

/**
 * Combination function that takes 1 or more inputs. If an odd
 * number of inputs are >= .5, then 1 is returned, otherwise 
 * (even number of inputs are >= .5), then 0 is returned.
 * 
 * If there are zero inputs, 0 is returned
 * 
 * Order of inputs is not important
 * 
 * Eg.
 * ( .4, .6 ) => 1
 * ( .6, .4 ) => 1
 * ( .4, .4 ) => 0
 * ( .6, .6 ) => 0
 * ( .6) => 1
 * ( .4) => 0
 * ( .4, .4, .4, .6) => 1
 * ( .4, .4, .6, .6) => 0
 * ( .4, .6, .6, .6) => 1
 * ( .6, .6, .6, .6) => 0
 */
class XORCombination extends InputCombinationFunction {

	def combineInputs(inputs: Seq[Double]): Double = {
		if(inputs.size == 0)
			return 0
		
		val booleans:Seq[Int] = inputs.map{(test) =>
			if(test < .5) 0	else 1
		}
		val sum = booleans.reduceLeft(_ + _)
		
		if(sum % 2 == 1)
			1
		else
			0
	}

}

object XORCombination extends XORCombination

