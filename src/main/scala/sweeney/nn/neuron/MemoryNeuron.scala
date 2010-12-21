package sweeney.nn
package neuron

import calc.normalization._
import calc.combination._

class MemoryNeuron extends Neuron with NeuronMemory{

	protected var _normalizationFunc:NormalizationFunction = NoNormalizationFunction
	
	def getValue:Double = {
		val rememberedValue = getRememberedValue
		normalizeOutput(rememberedValue)
	}
}
