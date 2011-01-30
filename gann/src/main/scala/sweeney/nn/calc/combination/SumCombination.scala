package sweeney.nn.calc.combination

/**
 * Simple combination function that just sums the input values 
 */
class SumCombination extends InputCombinationFunction{
	
	def combineInputs(inputs:Seq[Double]):Double = {
		if(inputs.isEmpty)
			0.0
		else
			sum(inputs)
	}
}
object SumCombination extends SumCombination