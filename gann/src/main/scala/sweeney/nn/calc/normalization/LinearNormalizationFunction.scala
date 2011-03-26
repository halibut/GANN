package sweeney.nn.calc.normalization

/**
 * Normalizes the input such that values less than the midpoint between min and max are minOutput, values
 * greater than maxInput are maxOutput, and values between minInput and maxInput are linearly
 * interpolated between minOutput and maxOutput
 */
class LinearNormalizationFunction extends NormalizationFunction{
	override def normalize(input:Double, minInput:Double, maxInput:Double, minOutput:Double = 0, maxOutput:Double = 1):Double = {
		val (iMin,iMax) = realMinMax(minInput, maxInput)
		val (oMin,oMax) = realMinMax(minOutput, maxOutput)
		
		if(input < iMin)
			-oMin
		else if(input > iMax)
			oMax	
		else{
			val iRange = iMax - iMin
			val linterp = (input - iMin) / iRange
			
			adjOutput(linterp, oMin, oMax)
		}
	}
	
	override def derivative(input:Double, minInput:Double, maxInput:Double, minOutput:Double = 0, maxOutput:Double = 1):Double = {
		val (iMin,iMax) = realMinMax(minInput, maxInput)
		val (oMin,oMax) = realMinMax(minOutput, maxOutput)
		
		if(input < iMin || input > iMax){
			0.0		//outside the input range, the slope is zero
		}
		else{
			(oMax-oMin) / (iMax-iMin)	//Inside the range the slope is linear
		}
	}
	
}
object LinearNormalizationFunction extends LinearNormalizationFunction