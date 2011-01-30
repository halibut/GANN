package sweeney.nn

import neuron._

class NeuralNetwork[T] {
	
	private var _layersDirty = true
	
	private var _inputMap:Map[T,InputNeuron] = Map()
	private var _outputMap:Map[T,OutputNeuron] = Map()
	private var _neuronMap:Map[T,Neuron] = Map()
	private var _neuronLayers:List[List[Neuron]] = List()

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
		for(keyAndVal <- inputValues){
			val (key,inputValue) = keyAndVal
			val inputNeuron = inputs(key)
			inputNeuron.value = inputValue 
		}
		
		//Calculate the neuron values for each layer
		for(layer <- _neuronLayers){
			//Temporarily story the values of all
			//input neurons for this layer
			for(neuron <- layer){
				neuron.getValuesOfInputs()
			}
			
			//Now, calculate the output value for 
			//all the neurons in the layer
			for(neuron <- layer){
				neuron.getValue
			}
		}
		
		_outputMap.map{(keyAndNeuron) =>
			val (key, neuron) = keyAndNeuron
			neuron.getValuesOfInputs()
			(key, neuron.getValue)
		}
	}
}