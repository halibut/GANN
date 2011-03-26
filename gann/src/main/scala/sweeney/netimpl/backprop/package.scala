package sweeney.netimpl

import sweeney.nn._
import sweeney.nn.neuron._

package object backprop {

	//Convenience methods for creating Output Neurons
	def BackPropOutput[T](key:T)(initFunc:(BackPropOutputNeuron)=>Unit)(implicit nn:NeuralNetwork[T]):BackPropOutputNeuron = {
		val newNeuron = new BackPropOutputNeuron();
		initFunc(newNeuron);
		nn.addOutput(key, newNeuron)
		newNeuron
	}
	
	//Convenience Methods for creating other types of neurons (Combiner and Memory)
	def BackProp[T](key:T)(initFunc:(BackPropNeuron)=>Unit)(implicit nn:NeuralNetwork[T]):Neuron = {
		val newNeuron = new BackPropNeuron();
		initFunc(newNeuron);
		nn.addNeuron(key, newNeuron)
		newNeuron
	}

}