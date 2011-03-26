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
	
	def setWeight(index:Int,weight:Double){
		val (inputNeuron,curWeight) = _inputs(index)
		_inputs(index) = ((inputNeuron,weight))
	}
	
	def getWeight(index:Int):Double = {
		_inputs(index)._2
	}
	
	def loadInputValues(){
		val newTempInputs = _inputs.map{neuronAndWeight =>
			val (neuron,weight) = neuronAndWeight
			neuron.getValue * weight
		}
		_tempInputValues = newTempInputs 
	}
}
 
 /**
  * Base trait that describes a neuron that takes 1 or more inputs and combines them
  * in some way.
  */
trait NeuronInputCombination {
	self:SimpleNeuron with sweeney.nn.neuron.NeuronInputs =>
	
	protected var _combineFunc:InputCombinationFunction;
	private var _combinedValue = 0.0
	private var _calcedCombination = false
	var bias:Double = 0.0

	
	def clearCombinedValue() = _calcedCombination = false
	
	def combineFunc_=(combineFunc:InputCombinationFunction){
		_combineFunc = combineFunc
	}
	def combineFunc():InputCombinationFunction = _combineFunc

	def combine() = {
		_combineFunc
	}
	
	def getCombinedInputs():Double = {
		if(!_calcedCombination){
			_combinedValue = combineFunc.combineInputs(_tempInputValues)
			_calcedCombination = true
		}
		_combinedValue
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
	protected var _calcedMemValue = false
	
	def getRememberedValue():Double = {
		if(_tempInputValues.size < 2){
			0.0
		}
		else{
			if(!_calcedMemValue){
				val halfInd = _tempInputValues.size / 2
				val (first,second) = _tempInputValues.splitAt(halfInd)
				val activation = AverageCombination.combineInputs(first)
				val value = AverageCombination.combineInputs(second)
				if(activation >= _memActivation)
					_memValue = value
					
				_calcedMemValue = true
			}
			_memValue
		}
	}
	
	def clearMemory(){
		_calcedMemValue = false
	}
}

trait NeuronActivation{
	self:SimpleNeuron =>
	
	protected var _normalizationFunc:NormalizationFunction;
	var minInput:Double = 0;
	var maxInput:Double = 1;
	var minOutput:Double = 0;
	var maxOutput:Double = 1;
	
	def normalizationFunc_=(normalizationFunc:NormalizationFunction){
		_normalizationFunc = normalizationFunc
	}
	def normalizationFunc():NormalizationFunction = _normalizationFunc
	
	def inputRange_=(range:(Double,Double)):Unit = { 
		minInput = range._1
		maxInput = range._2
	}
	def inputRange():(Double,Double) = { 
		(minInput,maxInput)
	}
	
	def outputRange_=(range:(Double,Double)):Unit = { 
		minOutput = range._1
		maxOutput = range._2
	}
	def outputtRange():(Double,Double) = { 
		(minOutput,maxOutput)
	}
	
	def normalizeOutput(unNormalized:Double):Double = {
		_normalizationFunc.normalize(unNormalized, minInput, maxInput, minOutput, maxOutput)
	}
	
	def derivOutput(unNormalized:Double):Double = {
		_normalizationFunc.derivative(unNormalized, minInput, maxInput, minOutput, maxOutput)
	}
}

trait SimpleNeuron {
	
	def reset():Unit
	
	def getValue:Double
}

trait NeuronWithInputs extends SimpleNeuron with NeuronInputs

trait Neuron extends NeuronWithInputs with NeuronActivation