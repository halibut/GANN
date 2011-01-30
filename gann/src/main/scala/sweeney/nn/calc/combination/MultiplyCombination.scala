package sweeney.nn.calc.combination

/**
 * Averages the first half of the inputs and multiplies by the average of the
 * second half of the inputs
 * 
 * For an odd number of inputs, the odd input value will be part of the
 * second half average.
 */
class MultiplyCombination extends InputCombinationFunction{
	
	def combineInputs(inputs:Seq[Double]):Double = {
		splitAndCombine2Seq(inputs,0)(average(_))(_ * _)
	}
}
object MultiplyCombination extends MultiplyCombination