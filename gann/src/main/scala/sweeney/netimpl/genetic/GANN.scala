package sweeney.netimpl.genetic
import sweeney.nn.neuron._

import sweeney.ga._
import sweeney.nn._


abstract class GANN[I, NK, N <: NeuralNetwork[NK]] extends 
	GeneticAlgorithm[I] with GAComponents[I]{
	
	def createNetwork():N;
	def setupNetworkForIndividual(network:N,individual:I);
	def calculateNetworkFitness(nework:N):Double;
	def generateHiddenNeuronKey(layer:Int,index:Int):NK;
	
	def trainNetwork():N = {
		trainGenerations()
		
		val best = getPopulation(0,1).head
		
		val network = createNetwork()
		setupNetworkForIndividual(network, best._1.expressedIndividual)
		network
	}
	
	
	def createInput():()=>InputNeuron = null;
	def createOutput():()=>OutputNeuron = null;
	def createCombiner():()=>CombinerNeuron = null;
	def createMemory():()=>MemoryNeuron = null;
}


