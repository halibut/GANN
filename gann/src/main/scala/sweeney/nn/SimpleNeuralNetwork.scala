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
		val inputKeys:IndexedSeq[T], 
		val outputKeys:IndexedSeq[T], 
		val hiddenLayers:IndexedSeq[Int]) extends NeuralNetwork[T] {
	
	require(!inputKeys.isEmpty,"Input layer must have at least 1 neuron.")
	require(!outputKeys.isEmpty,"Output layer must have at least 1 neuron.")
	require(hiddenLayers.forall(_ > 0),"Hidden layers must have at least 1 neuron. - " + hiddenLayers)
	
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

}