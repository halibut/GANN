package sweeney.nn.calc.combination

/**
 * Computes the average of all the input values 
 */
class AverageCombination extends InputCombinationFunction{
	
	def combineInputs(inputs:Seq[Double]):Double = {
		if(inputs.isEmpty){
			0
		}
		else{
			average(inputs)
		}
	}
}
object AverageCombination extends AverageCombination