package sweeney.nn.calc.normalization

/**
 * Defines a function for normalizing a value between -1 and 1
 */
trait NormalizationFunction {
	
	protected def realMinMax(first:Double,second:Double):(Double,Double) = {
		if(first<= second) (first,second) else (second,first)
	}
	
	protected def adjOutput(value:Double,minOutput:Double, maxOutput:Double):Double = {
		val oRange = maxOutput - minOutput
		value * oRange + minOutput
	}
	
	def normalize(input:Double, minInput:Double, maxInput:Double, minOutput:Double = 0, maxOutput:Double = 1):Double;
	
	def derivative(input:Double, minInput:Double, maxInput:Double, minOutput:Double = 0, maxOutput:Double = 1):Double;
}
