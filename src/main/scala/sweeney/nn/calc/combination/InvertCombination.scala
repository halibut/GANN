package sweeney.nn.calc.combination

/**
 * Averages all inputs and then inverts the average (1 / avg)
 */
class InvertCombination extends InputCombinationFunction{
	
	def combineInputs(inputs:Seq[Double]):Double = {
		if(inputs.size == 0){
			0
		}
		else{
			val avg = average(inputs)
			1 / avg
		}
	}
}
object InvertCombination extends InvertCombination