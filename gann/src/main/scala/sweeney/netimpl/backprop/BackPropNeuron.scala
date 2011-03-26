package sweeney.netimpl.backprop

import sweeney.nn.neuron._

import scala.collection.mutable.ArrayBuffer

trait BackProp {
	self:CombinerNeuron =>

	private var _error:Double = 0.0
	
	protected[backprop] def error():Double = {_error}
	protected[backprop] def accumError(error:Double) = {_error += error}
	protected[backprop] def clearError(){ _error = 0.0 }
	
	protected[backprop] def adjustWeights(learnFactor:Double){
		val preNormalizedValue = getCombinedInputs
		val derivValue = this.derivOutput(preNormalizedValue)
		val totalError = error
		val adjustedError = derivValue * totalError
		val newBias = this.bias + adjustedError * learnFactor 
		bias = newBias
		
		val newInputs = _inputs.map{neuronWeight =>
			val (neuron,weight) = neuronWeight
			val adjustment = neuron.getValue * adjustedError
			(neuron, weight + learnFactor * adjustment)
		}
		_inputs = newInputs 
	}
	
	protected[backprop] def getForwardWeight(forwardNeuron:NeuronInputs):Double = {
		 val neuronAndWeight = forwardNeuron.inputsWithWeights.find(_._1 eq this)
		 neuronAndWeight.get._2
	}
}

class BackPropNeuron extends CombinerNeuron with BackProp;

class BackPropOutputNeuron extends OutputNeuron with BackProp;