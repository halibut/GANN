package sweeney

import nn.neuron._
import nn.calc.combination._

package object nn {

	def NeuralNetwork[T](initFunc:(NeuralNetwork[T])=>Unit):NeuralNetwork[T] = {
		val network = new NeuralNetwork[T];
		initFunc(network);
		
		network
	}
	
	def UpdateNetwork[T](network:NeuralNetwork[T])(updateFunc:(NeuralNetwork[T])=>Unit):Unit = {
		updateFunc(network);
	}
	
	//Convenience methods for creating Output Neurons
	def Output[T](key:T)(initFunc:(OutputNeuron)=>Unit)(implicit nn:NeuralNetwork[T]):OutputNeuron = {
		val newNeuron = new OutputNeuron();
		initFunc(newNeuron);
		nn.addOutput(key, newNeuron)
		newNeuron
	}
	def OutputRef[T](key:T)(implicit nn:NeuralNetwork[T]):OutputNeuron = {
		nn.outputs(key)
	}
	
	//Convenience Methods for creating Input Neurons
	def Input[T](key:T)(implicit nn:NeuralNetwork[T]):InputNeuron = {
		val newNeuron = new InputNeuron();
		nn.addInput(key, newNeuron)
		newNeuron
	}
	def InputRef[T](key:T)(implicit nn:NeuralNetwork[T]):InputNeuron = {
		nn.inputs(key)
	}
	
	
	//Convenience Methods for creating other types of neurons (Combiner and Memory)
	def Combiner[T](key:T)(initFunc:(CombinerNeuron)=>Unit)(implicit nn:NeuralNetwork[T]):Neuron = {
		val newNeuron = new CombinerNeuron();
		initFunc(newNeuron);
		nn.addNeuron(key, newNeuron)
		newNeuron
	}
	def Memory[T](key:T)(initFunc:(MemoryNeuron)=>Unit)(implicit nn:NeuralNetwork[T]):Neuron = {
		val newNeuron = new MemoryNeuron();
		initFunc(newNeuron);
		nn.addNeuron(key, newNeuron)
		newNeuron
	}
	def NeuronRef[T](key:T)(implicit nn:NeuralNetwork[T]):Neuron = {
		nn.neurons(key)
	}

	
	//Convenience methods for setting up combination neurons
	def combineFunc_=(combine:InputCombinationFunction)(implicit nic:NeuronInputCombination){ 
		nic.combineFunc_=(combine) 
	}
	def combineFunc(implicit nic:NeuronInputCombination):InputCombinationFunction = { 
		nic.combineFunc
	}
	
	//Convenience methods for setting up neurons with inputs
	def addInput(weight:Double)(input:SimpleNeuron)(implicit ni:NeuronInputs):Unit = 
		ni.addInput((input, weight))
	def clearInputs()(implicit ni:NeuronInputs):Unit = 
		ni.clearInputs

}