package sweeney.nn

import sweeney.nn.neuron._
import sweeney.nn.calc.combination._

/**
 * Creates a traditional directed acyclical Neural Network, with a layer for inputs,
 * a layer for outputs, and a number of hidden layers
 * 
 * All outputs from the previous layer's neurons are connected to the inputs of each 
 * neuron in the next layer. The output of a neuron is calculated by summing the 
 * ouput of each input neuron multiplied by its weight, and then the resulting value
 * is passed to a normalization function.
 * 
 * @tparam T is the type of the key used to retrieve input and output neurons
 * @param inputKeys an IndexedSeq of keys used to identify the input neurons
 * @param outputKeys an IndexedSeq of keys used to identify the output neurons
 * @param hiddenLayers an IndexedSeq[Int] that describes the number of hidden layers,
 * and the number of neurons in each layer. For example: IndexedSeq(1,2,3) would
 * produce 3 hidden layers, the first containing 1 neuron, the second containing 2,
 * and the last containing 3.
 */
abstract class SimpleNeuralNetwork[T](
		val inputKeys:Seq[T], 
		val outputKeys:Seq[T], 
		val hiddenLayers:Seq[Int]) extends NeuralNetwork[T] {
	
	require(!inputKeys.isEmpty,"Input layer must have at least 1 neuron.")
	require(!outputKeys.isEmpty,"Output layer must have at least 1 neuron.")
	require(hiddenLayers.forall(_ > 0),"Hidden layers must have at least 1 neuron. - " + hiddenLayers)
	
	/**
	 * The total number of weights in the neural network
	 */
	val weightsLength = {
		val lengths = hiddenLayers ++ Seq(outputKeys.size)
		var prevLayerLength = inputKeys.size
		
		var weights = 0
		for(layerLength <- lengths){
			weights += prevLayerLength * layerLength;
			prevLayerLength = layerLength;
		}
		weights
	}

	/**
	 * The total number of biases in the neural network 
	 * (1 for each neuron with an input combination function, so 
	 * 1 for each hidden and output neuron)
	 */
	val biasesLength = {
		outputKeys.size + hiddenLayers.reduceLeft(_ + _)
	}

	//Constructor initializes the NeuralNetwork with inputs, hidden layers, and outputs
	{
		implicit val network:NeuralNetwork[T] = this
		//Create the inputs
		var prevLayer:Seq[SimpleNeuron] = inputKeys.map{ Input(_) }
		
		//Create neurons for each hidden layer
		for(layer <- 0 until hiddenLayers.size){
			val curLayer = for(i <- 0 until hiddenLayers(layer)) yield {
				Combiner(generateHiddenNeuronKey(layer,i)){ implicit n =>
					for(neuron <- prevLayer){
						sweeney.nn.addInput()(neuron)
					}
				}
			}
			prevLayer = curLayer
		} 
		
		//Create the outputs
		outputKeys.foreach{ key =>
			Output(key){ implicit n => 
				for(neuron <- prevLayer){
					sweeney.nn.addInput()(neuron)
				}
			}
		}
		
		determineLayers()
	}
	
	/**
	 * Method to generate unique keys for the hidden neuron specified by the
	 * layer and index in that layer
	 * @param layer the hidden layer that the neuron is in (starts at 0)
	 * @param index the index of the neuron in the specified hidden layer (starts at 0)
	 * @return A unique key that identifies the hidden layer.
	 */
	def generateHiddenNeuronKey(layer:Int,index:Int):T;
	
	/**
	 * Updates all the weights in the neural network
	 * @param weights an IndexedSeq[Double] that contains all the weights for all inputs to all
	 * neurons in the network. The order of weights is constant starting from the first hidden
	 * layer's first neuron and updating all of it's input weights (order specified by inputKeys
	 * constructor argument). The update will continue until all weights have been updated for 
	 * that layer, and then move onto the next layer. Lastly, the output layer will be updated
	 * (in the order specified by outputKeys constructor agrument).
	 */
	def setWeights(weights:IndexedSeq[Double]){
		require(weightsLength <= weights.size, "Expected at least "+weightsLength+" weights, but got "+weights.size+".")
		determineLayers()
		
		var weightIndex = 0
		for(layer <- _neuronLayers){
			for(neuron <- layer; inputIndex <- 0 until neuron.inputs.size){
				val weight = weights(weightIndex)
				neuron.setWeight(inputIndex, weight)
				weightIndex += 1
			}
		}
		
		for(key <- outputKeys){
			val neuron = _outputMap(key);
			for(inputIndex <- 0 until neuron.inputs.size){
				val weight = weights(weightIndex)
				neuron.setWeight(inputIndex, weight)
				weightIndex += 1
			}
		}
	}
	
		/**
	 * Updates all the weights in the neural network
	 * @param weights an IndexedSeq[Double] that contains all the weights for all inputs to all
	 * neurons in the network. The order of weights is constant starting from the first hidden
	 * layer's first neuron and updating all of it's input weights (order specified by inputKeys
	 * constructor argument). The update will continue until all weights have been updated for 
	 * that layer, and then move onto the next layer. Lastly, the output layer will be updated
	 * (in the order specified by outputKeys constructor agrument).
	 */
	def getWeights():IndexedSeq[Double] = {
		var weights:IndexedSeq[Double] = IndexedSeq()
		determineLayers()
		
		for(layer <- _neuronLayers){
			for(neuron <- layer){
				weights = weights ++ neuron.inputsWithWeights.map(_._2)
			}
		}
		
		for(key <- outputKeys){
			val neuron = _outputMap(key);
			weights = weights ++ neuron.inputsWithWeights.map(_._2)
		}
		
		weights
	}

	/**
	 * Get the bias value of each neuron in a sequence, starting with
	 * the first neuron in the first hidden layer and ending with the
	 * last ouput neuron in the output layer
	 */
	def getBiases():Seq[Double] = {
		determineLayers()
		
		val outputLayer:Seq[Neuron] = outputKeys.map(key => _outputMap(key))
		val combineLayers:Seq[Seq[Neuron]] = _neuronLayers ++ Seq(outputLayer)
		
		combineLayers.flatten.map(_.bias)
	}
	
	/**
	 * Set the bias value of each neuron from the sequence, starting with
	 * the first neuron in the first hidden layer and ending with the
	 * last ouput neuron in the output layer
	 */
	def setBiases(biases:Seq[Double]):Unit = {
		require(biasesLength <= biases.size, "Expected at least "+biasesLength+" weights, but got "+biases.size+".")
		determineLayers()
		
		val outputLayer:Seq[Neuron] = outputKeys.map(key => _outputMap(key))
		val combineLayers:Seq[Seq[Neuron]] = _neuronLayers ++ Seq(outputLayer)
		
		//Zip the bias with the neuron and then set all the biases
		for(neuronBias <- combineLayers.flatten.zip(biases)){
			val(neuron,bias) = neuronBias
			neuron.bias = bias
		}
	}
	
	override def calculate(inputs:Map[T,Double]):Map[T,Double] = {
		neurons.foreach(neuron => neuron.reset())
		outputs.foreach(neuron => neuron.reset())
		super.calculate(inputs)
	}

	
	
	override def createMemory():MemoryNeuron = {
		throw new UnsupportedOperationException("Simple networks do not have MemoryNeurons.");
	}
	
	
	private implicit def castAsActivationNeuron(neuron:Neuron):NeuronInputCombination = {
		neuron.asInstanceOf[NeuronInputCombination]
	}
	
	override def toString():String = {
		val sb = new StringBuffer()
		sb.append("Network{\n")
		.append("  Inputs(").append(inputKeys.size).append(")\n")
		.append("  Hidden Layers (").append(hiddenLayers.size).append(")\n")
		
		var i = 1
		for(layer <- _neuronLayers){
			sb.append("    Layer(").append(i).append(")\n")
			for(neuron <- layer){
				sb.append("      N(")
				.append(neuron.inputsWithWeights.map(_._2).mkString("w(",",",")"))
				.append("->b(").append(neuron.bias).append("))")
				.append("\n")
			}
			i += 1
		}
		
		sb.append("  Output Layer(").append(outputKeys.size).append(")\n")
		for(n <- outputKeys.map(key => _outputMap(key))){
			sb.append("    O(")
			.append(n.inputsWithWeights.map(_._2).mkString("w(",",",")"))
			.append("->b(").append(n.bias).append("))")
			.append("\n")
		}
		
		sb.toString
	}
}