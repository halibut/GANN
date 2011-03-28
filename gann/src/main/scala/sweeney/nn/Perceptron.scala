package sweeney.nn

abstract class Perceptron[T](
		val inputKeys:Seq[T], 
		val outputKeys:Seq[T], 
		val hiddenLayers:Seq[Int])
	extends NeuralNetwork[T] with SimpleNeuralNetwork[T]

object Perceptron{
	def getWeightLength(inputsSize:Int, layersSizes:Seq[Int], outputsSize:Int):Int = {
		var lastLayer = inputsSize
		
		val combineLayers = layersSizes ++ Seq(outputsSize)
		
		var length = 0
		for(layer <- combineLayers){
			length += layer * lastLayer
			lastLayer = layer
		}
		
		length
	}
	
	def getBiasesLength(layersSizes:Seq[Int], outputsSize:Int):Int = {
		layersSizes.reduceLeft(_+_) + outputsSize
	}
}
