package sweeney.ga
package fitness

import persistence._

trait GAFitnessTestProvider[I] {
this: GAPopulationPersistenceProvider[I] =>

	def fitnessTester:GAFitnessTester;

	trait GAFitnessTester{
		
		def determinePopulationFitness();
	}

}