package sweeney.gann.example.tank.network

import sweeney.nn._
import sweeney.nn.neuron._
import sweeney.ga._
import sweeney.netimpl.genetic._
import sweeney.netimpl.genetic.simple._

abstract class TankNetwork(val inputKeys:Seq[String], val outputKeys:Seq[String], val hiddenLayers:Seq[Int]) 
	extends GANeuralNetwork[WeightBiasGeneticCode,String] with SimpleNeuralNetwork[String] with StringKey{

	
	//Because we use the sigmoid function for activation, which asymptotically 
	//converges to 0 and 1, it helps to expand the output range if the values
	//we're looking for are near 0 and 1. It will allow the outputs to converge
	//more quickly to the correct values during training.
	override def createOutput():OutputNeuron = {
		val neuron = new OutputNeuron()
		neuron.minOutput = -1.1
		neuron.maxOutput = 1.1
		neuron
	}
	override def createCombiner():CombinerNeuron = {
		val neuron = new CombinerNeuron()
		neuron.minOutput = -1.1
		neuron.maxOutput = 1.1
		neuron
	}
}