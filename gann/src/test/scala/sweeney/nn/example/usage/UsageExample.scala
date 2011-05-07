package sweeney.nn.example.usage

import sweeney.nn._
import sweeney.nn.{NeuralNetwork}
import sweeney.nn.neuron._
import sweeney.nn.calc.combination._

object UsageExample {
	def main(args : Array[String]) : Unit = {

		//Creates network consisting of 5 neurons
		
		val network = NeuralNetwork[String]{implicit nn =>
			Output("output1"){implicit n =>
				//Creates 2 input neurons
				val input1 = Input("Input 1")
				val input2 = Input("Input 2")
			
				addInput(1.0){
					Combiner("Intermediate1"){implicit n =>
						combineFunc = SumCombination
						addInput()(input1)
						addInput()(input2)
					}
				}
				
				addInput(3.0){
					Combiner("Intermediate2"){implicit n =>
						combineFunc = AverageCombination
						addInput()(input1)
						addInput()(input2)
					}
				}
				
				combineFunc = MultiplyCombination
			}
		}
		
	}

}
