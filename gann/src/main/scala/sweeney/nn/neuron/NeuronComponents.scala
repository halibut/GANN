package sweeney.nn
package neuron

import calc.normalization._
import calc.combination._

import scala.collection.mutable.ArrayBuffer


trait NeuronInputs{
	self:SimpleNeuron =>
	
	protected var _inputs:ArrayBuffer[(SimpleNeuron,Double)] = ArrayBuffer()
	protected var _tempInputValues:ArrayBuffer[Double] = ArrayBuffer()
	
	def clearInputs(){ _inputs.clear }
	def addInput(inputAndWeight:(SimpleNeuron,Double)){
		_inputs += inputAndWeight
	}
	def inputs():Seq[SimpleNeuron] = { _inputs.map(_._1) }
	def inputsWithWeights():Seq[(SimpleNeuron,Double)] = { _inputs.toSeq }
	
	def getValuesOfInputs(){
		_tempInputValues = _inputs.map{neuronAndWeight =>
			val (neuron,weight) = neuronAndWeight
			neuron.getValue * weight
		}
	}
}
 
 /**
  * Base trait that describes a neuron that takes 1 or more inputs and combines them
  * in some way.
  */
trait NeuronInputCombination {
	self:SimpleNeuron with sweeney.nn.neuron.NeuronInputs =>
	
	protected var _combineFunc:InputCombinationFunction;
	
	def combineFunc_=(combineFunc:InputCombinationFunction){
		_combineFunc = combineFunc
	}
	def combineFunc():InputCombinationFunction = _combineFunc
	
	def combine() = {
		_combineFunc
	}
	
	def getCombinedInputs():Double = {
		combineFunc.combineInputs(_tempInputValues)
	}
}

/**
 * A special type of neuron that can remember the value
 * that it calculates based on activation of a second input value 
 */
trait NeuronMemory{
	self:SimpleNeuron with sweeney.nn.neuron.NeuronInputs =>
	
	protected var _memValue = 0.0
	protected var _memActivation = 0.5
	
	def getRememberedValue():Double = {
		if(_tempInputValues.size < 2){
			0.0
		}
		else{
			val halfInd = _tempInputValues.size / 2
			val (first,second) = _tempInputValues.splitAt(halfInd)
			val activation = AverageCombination.combineInputs(first)
			val value = AverageCombination.combineInputs(second)
			if(activation >= _memActivation)
				_memValue = value
			
			_memValue
		}
		
	}
}

trait NeuronActivation{
	self:SimpleNeuron =>
	
	protected var _normalizationFunc:NormalizationFunction;
	protected var _minInput:Double = 0;
	protected var _maxInput:Double = 1;
	protected var _minOutput:Double = 0;
	protected var _maxOutput:Double = 1;
	
	def normalizationFunc_=(normalizationFunc:NormalizationFunction){
		_normalizationFunc = normalizationFunc
	}
	def normalizationFunc():NormalizationFunction = _normalizationFunc
	
	def minInput_=(minInput:Double) = {_minInput = minInput}
	def minInput():Double = { _minInput }
	
	def maxInput_=(maxInput:Double) = {_maxInput = maxInput}
	def maxInput():Double = { _maxInput }
	
	def minOutput_=(minOutput:Double) = {_minOutput = minOutput}
	def minOutput():Double = { _minOutput }
	
	def maxOutput_=(maxOutput:Double) = {_maxOutput = maxOutput}
	def maxOutput():Double = { _maxOutput }
	
	def inputRange_=(range:(Double,Double)):Unit = { 
		minInput_=(range._1)
		maxInput_=(range._2)
	}
	def inputRange():(Double,Double) = { 
		(minInput,maxInput)
	}
	
	def outputRange_=(range:(Double,Double)):Unit = { 
		minOutput_=(range._1)
		maxOutput_=(range._2)
	}
	def outputtRange():(Double,Double) = { 
		(minOutput,maxOutput)
	}
	
	def normalizeOutput(unNormalized:Double):Double = {
		_normalizationFunc.normalize(unNormalized, _minInput, _maxInput, _minOutput, _maxOutput)
	}
}

trait SimpleNeuron {
	
	def getValue:Double
}

trait NeuronWithInputs extends SimpleNeuron with NeuronInputs

trait Neuron extends NeuronWithInputs with NeuronActivation