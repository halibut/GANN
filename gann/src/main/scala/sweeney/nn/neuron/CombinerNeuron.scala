package sweeney.nn
package neuron

import calc.normalization._
import calc.combination._

class CombinerNeuron extends Neuron with NeuronInputCombination{

	protected var _combineFunc:InputCombinationFunction = SumCombination
	protected var _normalizationFunc:NormalizationFunction = SigmoidNormalizationFunction
	
	def getValue:Double = {
		val combinedValue = _combineFunc.combineInputs(_tempInputValues)
		normalizeOutput(combinedValue)
	}
	
	def reset():Unit = {
		clearCombinedValue()
	}
}

