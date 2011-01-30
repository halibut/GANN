package sweeney.nn.calc.combination

/**
 * Ignores the input values and just returns the total number of inputs
 */
class CountCombination extends InputCombinationFunction{
	
	def combineInputs(inputs:Seq[Double]):Double = {
		inputs.size
	}
}
object CountCombination extends CountCombination