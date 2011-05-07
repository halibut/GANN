package sweeney.netimpl.genetic

import sweeney.ga.GeneticCode
import sweeney.ga.fitness.FitnessCalculator
import sweeney.ga.impl.PagedInMemoryGA
import sweeney.nn.NeuralNetwork


abstract class PagedGANN[I,NK, N <: NeuralNetwork[NK]] extends GANN[I,NK,N] with PagedInMemoryGA[I]{
	
	override def createFitnessCalculator():FitnessCalculator[I] = {
	    new FitnessCalculator[I] {
	    	val network:N = createNetwork()
	      
	    	override def calculateFitness(gc:GeneticCode[I],existingFitness:Double):Double = {
	    		setupNetworkForIndividual(network,gc.expressedIndividual)
	    		
	    		existingFitness + calculateNetworkFitness(network)
	    	}
	    }
	}
	
}


