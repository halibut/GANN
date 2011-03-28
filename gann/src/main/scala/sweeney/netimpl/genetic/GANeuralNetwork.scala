package sweeney.netimpl.genetic

import sweeney.ga._
import sweeney.nn._


abstract class GANeuralNetwork[I,NK] extends NeuralNetwork[NK] with GeneticAlgorithm[I]{
	
	def setupNetworkForIndividual(individual:I):Unit;
	
	def trainNetwork(){
		trainGenerations()
		
		val best = getPopulation.last
		setupNetworkForIndividual(best.code.expressedIndividual)
	}
	
	
}


