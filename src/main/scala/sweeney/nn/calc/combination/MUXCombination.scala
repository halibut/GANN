package sweeney.nn.calc.combination

import scala.collection.Seq

/**
 * Combination function that takes 3 inputs. The first and second inputs 
 * are selected by the third input as the output. If the 3rd input is < .5, 
 * then the first input is returned, otherwise the second input is returned.
 * Eg. first | second | third | output
 *      10   |   5    |  0.25 |  10
 *      10   |   5    |  0.75 |  5
 *      10   |   5    |  0.5  |  5
 *      
 * If there are more than 3 inputs, an attempt will be made to average 3 sets
 * from the input values. For instance, if there are 6 inputs, (1,2), (3,4), (5,6)
 * 
 * An input count that isn't divisible by 3 will be combined so that the remaining
 * inputs are averaged as part of the third input. For instance, if there are 
 * 4 inputs: (1), (2), (3,4);  5 inputs: (1), (2), (3,4,5);
 * 7 inputs (1,2), (3,4), (5,6,7);   etc...
 * 
 * If there are fewer than 3 inputs, the inputs cannot be combined and a value 
 * of 0.0 is returned
 */
class MUXCombination extends InputCombinationFunction {

	def combineInputs(inputs: Seq[Double]): Double = {
		splitAndCombine3Seq(inputs, 0)(average){(i1,i2,select) =>
			if(select < .5) i1 else i2
		}
	}

}

object MUXCombination extends MUXCombination