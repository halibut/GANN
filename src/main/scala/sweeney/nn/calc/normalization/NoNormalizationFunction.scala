package sweeney.nn.calc.normalization


/**
 * A special function that does not perform any normalization. The input
 * is passed straight through without changing.
 */
class NoNormalizationFunction extends NormalizationFunction{
	def normalize(input:Double, minInput:Double, maxInput:Double, minOutput:Double = 0, maxOutput:Double = 1):Double = {
		input
	}
}
object NoNormalizationFunction extends NoNormalizationFunction