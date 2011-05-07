package sweeney.ga
package impl.comp

import fitness.PagedFitnessTest
import gen.PagedNewGenerationProvider
import gen.RouletteSelection
import persistence.GAPopulationPersistenceProvider

trait PagedGA[I] extends GeneticAlgorithm[I] with GAComponents[I] with 
	PagedFitnessTest[I] with PagedNewGenerationProvider[I] with
	RouletteSelection[I]{

	def pageSize = 50
	def concurrentPages = 1
    
    override def fitnessTestPageSize = pageSize
    override def genCreatorPageSize = pageSize
    
    override def fitnessTestConcurrentPages = concurrentPages
    override def genCreatorConcurrentPages = concurrentPages
}