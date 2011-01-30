package sweeney.nn.calc.normalization

/**
 * Normalizes the input according to the sigmoid function, which is an S shaped
 * curve based on the input values.
 */
class SigmoidNormalizationFunction extends NormalizationFunction{
	
	def normalize(input:Double, minInput:Double, maxInput:Double, minOutput:Double = 0, maxOutput:Double = 1):Double = {
		val (iMin,iMax) = realMinMax(minInput, maxInput)
		val (oMin,oMax) = realMinMax(minOutput, maxOutput)
		
		val slopeAdj = (iMax-iMin) / 2
		val mid = slopeAdj + iMin
		
		val inputAdj = (input - mid) / slopeAdj
		
		val sigmaFuncVal = (1 / (1 + math.pow(math.E, -inputAdj)))
		
		adjOutput(sigmaFuncVal, minOutput, maxOutput)
	}
}
object SigmoidNormalizationFunction extends SigmoidNormalizationFunction