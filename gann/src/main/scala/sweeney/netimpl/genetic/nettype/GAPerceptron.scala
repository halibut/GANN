package sweeney.netimpl.genetic
package nettype

import sweeney.ga._
import sweeney.nn._
import sweeney.nn.neuron._
import sweeney.nn.calc.combination._

trait GAPerceptron[I,NK]{
this: GANN[I,NK,Perceptron[NK]] => 

  	val inputKeys:Seq[NK];
  	val outputKeys:Seq[NK];
  	val hiddenLayers:Seq[Int];
  	
  	//Get a reference to this object so we can pass it into
  	//the anonymous Perceptron and call methods on it
  	val gann = this
  	
	override def createNetwork():Perceptron[NK] = {
		new Perceptron(inputKeys, outputKeys, hiddenLayers){
			override def generateHiddenNeuronKey(layer:Int,index:Int):NK = {
				gann.generateHiddenNeuronKey(layer,index)
			}
			
			override def createInput():InputNeuron = {
				if(gann.createInput != null)
					gann.createInput()()
				else
					super.createInput()
			}
			override def createOutput():OutputNeuron = {
			  if(gann.createOutput != null)
					gann.createOutput()()
				else
					super.createOutput()
			}
			override def createCombiner():CombinerNeuron = {
			  if(gann.createCombiner != null)
					gann.createCombiner()()
				else
					super.createCombiner()
			}
			override def createMemory():MemoryNeuron = {
			  if(gann.createMemory != null)
					gann.createMemory()()
				else
					super.createMemory()
			}
		}
	}

  	
  	def weightsLength():Int = {
  		Perceptron.getWeightLength(inputKeys.size,hiddenLayers,outputKeys.size)
  	}
  	
  	def biasesLength():Int = {
  		Perceptron.getBiasesLength(hiddenLayers,outputKeys.size)
  	}
  	
  	//Because we use the sigmoid function for activation, which asymptotically 
	//converges to 0 and 1, it helps to expand the output range if the values
	//we're looking for are near 0 and 1. It will allow the outputs to converge
	//more quickly to the correct values during training.
	final override def createOutput():()=>OutputNeuron = ()=>{
		val neuron = new OutputNeuron()
		neuron.minOutput = minNeuronOutput
		neuron.maxOutput = maxNeuronOutput
		neuron
	}
	final override def createCombiner():()=>CombinerNeuron = ()=>{
		val neuron = new CombinerNeuron()
		neuron.minOutput = minNeuronOutput
		neuron.maxOutput = maxNeuronOutput
		neuron
	}
	
	def minNeuronOutput:Double = 0.0
	def maxNeuronOutput:Double = 1.0
}

