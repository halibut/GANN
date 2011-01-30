package sweeney.nn.calc.normalization


/**
 * Normalizes the input value such that if it is less than the midpoint between 
 * minInput and maxInput, it will return minOutput, otherwise it will return maxOutput
 */
class StepNormalizationFunction extends NormalizationFunction{
	def normalize(input:Double, minInput:Double, maxInput:Double, minOutput:Double = 0, maxOutput:Double = 1):Double = {
		val (iMin,iMax) = realMinMax(minInput, maxInput)
		val (oMin,oMax) = realMinMax(minOutput, maxOutput)
		
		val mid = ((iMax - iMin) / 2) + iMin
		if(input < mid)
			oMin
		else
			oMax	
	}
}
object StepNormalizationFunction extends StepNormalizationFunction