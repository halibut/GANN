package sweeney.nn
package neuron

class InputNeuron extends SimpleNeuron{

	private var _value = 0.0
	
	def value_=(value:Double) { this._value = value }
	def value = _value
	
	def getValue:Double = { this._value }
	
}

