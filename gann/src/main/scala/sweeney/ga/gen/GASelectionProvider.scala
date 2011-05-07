package sweeney.ga
package gen

trait GASelectionProvider[I] {
	def getPopulationSelector(generation:Int):GAPopulationSelector;

	trait GAPopulationSelector{
		/**
		 * @param numMates the number of mates to select from the population
		 * @return a sequence of selected mates (whose size is equal to numMates)
		 */
		def selectMates(numMates:Int):Seq[GeneticCode[I]]
	}

}