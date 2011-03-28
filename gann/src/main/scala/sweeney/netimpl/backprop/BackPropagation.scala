package sweeney.netimpl.backprop

import sweeney.nn._
import sweeney.nn.neuron._

trait BackPropagation[T] {
	self:NeuralNetwork[T] with SimpleNeuralNetwork[T] =>
	
	//Because we use the sigmoid function for activation, which asymptotically 
	//converges to 0 and 1, it helps to expand the output range if the values
	//we're looking for are near 0 and 1. It will allow the outputs to converge
	//more quickly to the correct values during training.
	override def createOutput():OutputNeuron = {
		val neuron = new BackPropOutputNeuron()
		neuron.minOutput = -.1
		neuron.maxOutput = 1.1
		neuron
	}
	override def createCombiner():CombinerNeuron = {
		new BackPropNeuron()
	}
	
	def train(expectedOutputs:Map[T,Double],learningFactor:Double) = {
		for(keyVal <- expectedOutputs){
			val (key,expected) = keyVal
			val output = outputs(key)
			val diff = expected - output.getValue
			output.clearError()
			output.accumError(diff)
			output.adjustWeights(learningFactor)
		}
		
		var outputLayer:Seq[Neuron] = outputs
		
		for(layer <- _neuronLayers.reverse){
			for(neuron <- layer){
				neuron.clearError()
				for(forwardNeuron <- outputLayer){
					val weight = neuron.getForwardWeight(forwardNeuron)
					val error = weight * forwardNeuron.error 
					neuron.accumError(error)
				}
			
				neuron.adjustWeights(learningFactor)
			}
			outputLayer = layer
		}
	}
	
	//Make casting to BackProp versions of neurons happen implicitly
	private implicit def castAsBackProp(n:OutputNeuron):BackProp = {
		n.asInstanceOf[BackProp]
	}
	private implicit def castAsBackProp(n:Neuron):BackProp = {
		n.asInstanceOf[BackProp]
	}
}