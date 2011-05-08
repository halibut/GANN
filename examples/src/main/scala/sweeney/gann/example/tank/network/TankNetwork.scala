package sweeney.gann.example.tank.network
import sweeney.gann.example.tank.game.Board

import sweeney.netimpl.genetic.nettype.GAPerceptron

import sweeney.nn._
import sweeney.nn.neuron._
import sweeney.ga._
import sweeney.netimpl.genetic._

abstract class TankNetwork(val inputKeys:Seq[String], val outputKeys:Seq[String], val hiddenLayers:Seq[Int]) 
	extends PagedGANN[WeightBiasGeneticCode,String,Perceptron[String]] 
	                  with GAPerceptron[WeightBiasGeneticCode,String]{

	override def generateHiddenNeuronKey(layer:Int,index:Int):String = {
		"Hidden-"+layer+"-"+index;
	}
	
	//Because we use the sigmoid function for activation, which asymptotically 
	//converges to 0 and 1, it helps to expand the output range if the values
	//we're looking for are near 0 and 1. It will allow the outputs to converge
	//more quickly to the correct values during training.
	override def minNeuronOutput:Double = -1.0
	override def maxNeuronOutput:Double = 1.0
	
}

