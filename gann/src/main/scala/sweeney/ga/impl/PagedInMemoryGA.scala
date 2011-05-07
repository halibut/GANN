package sweeney.ga
package impl

import comp.PagedGA
import fitness.PagedFitnessTest
import gen.PagedNewGenerationProvider
import gen.RouletteSelection
import persistence.InMemoryPopulationPersistence

trait PagedInMemoryGA[I] extends GeneticAlgorithm[I] with
	PagedGA[I] with InMemoryPopulationPersistence[I]{
  
}