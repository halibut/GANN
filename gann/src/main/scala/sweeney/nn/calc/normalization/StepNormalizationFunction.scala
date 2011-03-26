package sweeney.nn.calc.normalization


/**
 * Normalizes the input value such that if it is less than the midpoint between 
 * minInput and maxInput, it will return minOutput, otherwise it will return maxOutput
 */
class StepNormalizationFunction extends NormalizationFunction{
	override def normalize(input:Double, minInput:Double, maxInput:Double, minOutput:Double = 0, maxOutput:Double = 1):Double = {
		val (iMin,iMax) = realMinMax(minInput, maxInput)
		val (oMin,oMax) = realMinMax(minOutput, maxOutput)
		
		val mid = ((iMax - iMin) / 2) + iMin
		if(input < mid)
			oMin
		else
			oMax	
	}
	
	override def derivative(input:Double, minInput:Double, maxInput:Double, minOutput:Double = 0, maxOutput:Double = 1):Double = {
		0.0   //the slope of the step function is zero except at the step discontinuity (so we'll ignore that part)
	}
}
object StepNormalizationFunction extends StepNormalizationFunction