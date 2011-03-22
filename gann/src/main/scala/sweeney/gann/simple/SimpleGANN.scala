package sweeney.gann.simple

import sweeney.ga._
import sweeney.nn.neuron._
import sweeney.nn.calc.combination._

abstract class SimpleGANN[T](val inputKeys:IndexedSeq[T], val outputKeys:IndexedSeq[T], val hiddenLayers:IndexedSeq[Int], 
		initialPopulation:Seq[SimpleGeneticCode], generation:Int = 0) {
	
	

	def generateHiddenNeuronKey(layer:Int,index:Int):T;
	
	def initGeneValue():Double = { math.random }
	
	def mutateGeneValue(currentGene:Double):Double = {
		val newVal = currentGene + math.random - 0.5
		
		math.max(0, math.min(1, newVal))
	}
	
	class NetworkCalculation;
}

