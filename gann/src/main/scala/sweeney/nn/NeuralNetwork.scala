package sweeney.nn

import neuron._
import scala.actors.Futures._

class NeuralNetwork[T] {
	
	private var _layersDirty = true
	
	protected var _inputMap:Map[T,InputNeuron] = Map()
	protected var _outputMap:Map[T,OutputNeuron] = Map()
	protected var _neuronMap:Map[T,Neuron] = Map()
	protected var _neuronLayers:List[List[Neuron]] = List()

	/** Add an InputNeuron to the network */
	def addInput(key:T,neuron:InputNeuron):Unit = {	_inputMap += (key -> neuron) }
	
	/** Remove the specified InputNeuron from the map */
	def removeInput(key:T):Unit = { _inputMap -= key }
	
	/** Get the specified InputNeuron */
	def inputs(key:T):InputNeuron = { _inputMap(key) }
	
	/** Get all the InputNeurons */
	def inputs():Seq[InputNeuron] = { _inputMap.values.toSeq }
	
	/** Remove all InputNeurons from the network */
	def clearInputs():Unit = { _inputMap = Map() }

	/** Add an output neuron (CombinerNeuron) to the network */
	def addOutput(key:T, neuron:OutputNeuron):Unit = { _outputMap += (key -> neuron) }
	
	/** Remove the specified output neuron (CombinerNeuron) from the network */
	def removeOutput(key:T):Unit = { _outputMap -= key }
	
	/** Get the specified output neuron (CombinerNeuron) */
	def outputs(key:T):OutputNeuron = { _outputMap(key) }
	
	/** Get all the OutputNeurons */
	def outputs():Seq[OutputNeuron] = { _outputMap.values.toSeq }
	
	/** Remove all output neurons (CombinerNeuron) from the network */
	def clearOutputs():Unit = { _outputMap = Map() }
	
	/** Add a Neuron to the network */
	def addNeuron(key:T,neuron:Neuron):Unit = { 
		_neuronMap += (key -> neuron)
		_layersDirty = true
	}
	
	/** Remove the specified Neuron from the network */
	def removeNeuron(key:T):Unit = { 
		_neuronMap -= key
		_layersDirty = true
	}
	
	/** Get the specified neuron from the network */
	def neurons(key:T):Neuron = { _outputMap(key) }
	
	/** Get all the Neurons */
	def neurons():Seq[Neuron] = { _neuronMap.values.toSeq }
	
	/** Remove all Neurons from the network */
	def clearNeurons():Unit = { 
		_neuronMap = Map()
		_layersDirty = true
	}
	
	protected def determineLayers(){
		if(_layersDirty){
			println("Network layers are dirty. Calculating...")
			var nonTraversed:List[Neuron] = _neuronMap.values.toList
			var prevLayer:List[SimpleNeuron] = _inputMap.values.toList
			
			_neuronLayers = List()
			
			//Find all layers, starting with the ones closest to 
			//the input layer
			while(!nonTraversed.isEmpty){
				val layer = nonTraversed
						.filter((avail) => avail.inputs.exists( prevLayer.contains(_)) )
						.distinct
				
				_neuronLayers = _neuronLayers ::: List(layer)
				nonTraversed = nonTraversed.filterNot(layer.contains(_))
				prevLayer = layer
			}
					
			_layersDirty = false
		}
	}
	
	/**
	 * Takes the inputs, runs the neurons in the network, and returns the outputs
	 * @param inputValues a map containing the InputNeuron key-value pairs
	 * @return a map containing the output neuron key-value pairs
	 */
	def calculate(inputValues:Map[T,Double]):Map[T,Double] = {
		determineLayers()
		resetNetork();
		
		for(keyAndVal <- inputValues){
			val (key,inputValue) = keyAndVal
			val inputNeuron = inputs(key)
			inputNeuron.value = inputValue 
		}
		
		//Calculate the neuron values for each layer
		for(layer <- _neuronLayers){
			//Temporarily story the values of all
			//input neurons for this layer
			//This can be done in parallel
			val loadInputsFutures = for(neuron <- layer) yield {
				future{	neuron.loadInputValues() }
			}
			awaitAll(1000000,loadInputsFutures:_*)
			
			
			//Now, calculate the output value for 
			//all the neurons in the layer
			//This can also be done in parallel
			val calcFutures = for(neuron <- layer) yield {
				future{	neuron.getValue	}
			}
			awaitAll(1000000,calcFutures:_*)
		}
		
		val out = _outputMap.map{(keyAndNeuron) =>
			val (key, neuron) = keyAndNeuron
			neuron.loadInputValues()
			(key, neuron.getValue)
		}
		out
	}
	
	/**
	 * Gets called at the beginning of each call to the calculate method,
	 * and provides a chance for implementing networks to reset any internal
	 * state before calculating new values
	 */
	def resetNetork():Unit = { }
	
	def createInput():InputNeuron = {
		new InputNeuron()
	}
	def createOutput():OutputNeuron = {
		new OutputNeuron()
	}
	def createCombiner():CombinerNeuron = {
		new CombinerNeuron()
	}
	def createMemory():MemoryNeuron = {
		new MemoryNeuron();
	}
}