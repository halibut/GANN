package sweeney.nn

trait StringKey {
	self:SimpleNeuralNetwork[String] =>
	
	override def generateHiddenNeuronKey(layer:Int,index:Int):String = {
		"Hidden-"+layer+"-"+index;
	}
}