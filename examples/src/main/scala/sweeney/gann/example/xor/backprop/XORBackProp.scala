package sweeney.gann.example.xor.backprop

import sweeney.netimpl.backprop._
import sweeney.nn._

object XORBackProp {
	def main(args : Array[String]) : Unit = {
		val inputKeys = Seq("1","2");
		val outputKeys = Seq("out");
		val hiddenLayers = Seq(4,2)
		
		val testData = IndexedSeq(
			(Map("1" -> 1.0, "2" -> 0.0),Map("out" -> 1.0)),
			(Map("1" -> 0.0, "2" -> 0.0),Map("out" -> 0.0)),
			(Map("1" -> 0.0, "2" -> 1.0),Map("out" -> 1.0)),
			(Map("1" -> 1.0, "2" -> 1.0),Map("out" -> 0.0)))
  
 		val network = new Perceptron(inputKeys,outputKeys,hiddenLayers) with BackPropagation[String] with StringKey;
 		
 		//Initialize weights to random values
 		network.setWeights(for(i <- 0 until network.weightsLength) yield {3 * math.random - 1})
 		println(network.getWeights);
 		
 		var error = 0.0
 		var i = 0
 		var learnRate = .3
 		val iterations = 10000
 		while(i == 0 || (error >= 0.0001 && i < iterations) ){
 			error = 0
 			
 			var dataSet = if(1 % 2 == 0) testData else testData.reverse
 			for(data <- testData){
 				val actual = network.calculate(data._1)("out")
 				error += math.abs(data._2("out") - actual)
 				network.train(data._2, learnRate)
 			}
 			if(i % 100 == 0){
 				println(i+" error -> "+error
 						+" - weights -> " + network.getWeights
 						+" - biases -> " + network.getBiases);
 			}
 			
 			i+=1
 		}
		
 		println("\nFinished at: "+i)
 		println(network.toString)
 		
		for(data <- testData){
			println(data._1.toString+" -> "+network.calculate(data._1)("out"))
		}
	}
}
