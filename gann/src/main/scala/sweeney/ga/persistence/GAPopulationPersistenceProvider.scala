package sweeney.ga
package persistence

import scala.collection.immutable.IndexedSeq

trait GAPopulationPersistenceProvider[I] {

	def popPersistence:GAPopulationPersistence;
	
	trait GAPopulationPersistence{
		/**
		 * @return the most recent generation that has been persisted
		 */
		def getLatestGeneration:Int;
		
		/**
		 * @param gen the generation to get the size from
		 * @return the size of the specified population 
		 */
		def getPopulationSize(gen:Int):Option[Int];
	
		/**
		 * @param gen the generation to get the population from
		 * @param from the start index of the slice of the population to retrieve
		 * @param until the end index of the slice of the population to retrieve 
		 * a negative value specifies the end of the population
		 * @return the specified (slice of the) population 
		 */
		def getPopulation(gen:Int, from:Int=0, until:Int=(-1)):Option[Seq[(GeneticCode[I],Double)]]
		
		/**
		 * @param gen the generation to get the population from
		 * @param indexes a sequence of indexes to individuals in the population
		 * @return the specified population's individuals 
		 */
		def getPopulation(gen:Int, indexes:Seq[Int]):Option[Seq[(GeneticCode[I],Double)]]
		
		/**
		 * @param gen the generation to get the individual from
		 * @param index the index of an individual in the population
		 * @return the specified individual
		 */
		def getIndividual(gen:Int, index:Int):Option[(GeneticCode[I],Double)]
		
		/**
		 * @param gen specifies the generation to query 
		 * @param population the Seq of (GeneticCode,fitness) to insert or
		 * update in the generation's population
		 */
		def addOrUpdatePopulation(gen:Int, population:Seq[(GeneticCode[I],Double)])
		
		/**
		 * @param gen specifies the generation to query 
		 * @param from the index after wich all individuals will be deleted 
		 * (default is 0, meaning all individuals will be deleted
		 */
		def deletePopulation(gen:Int, from:Int=0)
		
		/**
		 * @param gen specifies the generation to query
		 * @return the sum of the fitness values for all the individuals in the population
		 */
		def getTotalPopulationFitness(gen:Int):Option[Double]
		
	}
}