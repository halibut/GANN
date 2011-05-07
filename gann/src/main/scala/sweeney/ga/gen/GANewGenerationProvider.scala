package sweeney.ga
package gen

import persistence._

trait GANewGenerationProvider[I] {
this: GAPopulationPersistenceProvider[I] with 
	GeneticAlgorithm[I] with GASelectionProvider[I]=>

	def newGenCreator:GANewGenerationCreator;
	
	/**
	 * @return The rate of genetic mutation when the current generation reproduces
	 */
	def mutationRate:Double = { 0.1 }
	
	/**
	 * @return The amount of genetic mutation that should occur during mutation
	 */
	def mutationRatio:Double = { 1.0 }
	
	/**
	 * @return The maximum difference between a mutated gene and the original
	 */
	def mutationSize:Double = { 1.0 }
	
	
	/**
	 * @return The rate of genetic crossover when the current generation reproduces
	 */
	def crossoverRate:Double = { 0.7 }
	
	/**
	 * @return the percentile of the population that will be carried over to the new
	 * generation without modification. Should be in the range of 0 to 1
	 */
	def elitistPercentile = {0.01}
	
	/**
	 * @return the Rate of carryover from one generation to the next
	 */
	def carryoverRate = {0.0}

	trait GANewGenerationCreator{
		
		final def createNewGeneration(currentGen:Int){
			implicit val currentGen = popPersistence.getLatestGeneration
			
			val numElites = math.max(0,math.min(populationSize-1, (populationSize * elitistPercentile).asInstanceOf[Int]))
			val numCarryovers = math.max(0,math.min(populationSize-numElites-1, (populationSize * carryoverRate).asInstanceOf[Int]))
			
			val actualElites = createElites(numElites)
			val actualCarryovers = createCarryovers(numElites,numCarryovers)
			
			val numCreated = populationSize - actualElites - actualCarryovers
			createNewIndividuals(numCreated)
		}
		
		def createElites(numElites:Int)(implicit currentGen:Int):Int = {
		    if(numElites == 0)
		        return 0;
		    
			val newGen = currentGen + 1
			val elites = popPersistence.getPopulation(currentGen,0,numElites)
			for(elitePop <- elites){
			    val individuals = elitePop.map(_._1).distinct
				popPersistence.addOrUpdatePopulation(newGen, individuals.map((_,0.0)))
				return individuals.size
			}
			return 0
		}
		def createCarryovers(start:Int,numCarryovers:Int)(implicit currentGen:Int):Int = {
			val newGen = currentGen + 1
			val indexRange = populationSize - start
			val carryoverIndices = for(i <- 0 until numCarryovers) yield {
				(math.random * indexRange).asInstanceOf[Int] + start
			}
			val carryovers = popPersistence.getPopulation(currentGen, carryoverIndices).get
			val individuals = carryovers.map(_._1).distinct
			popPersistence.addOrUpdatePopulation(newGen, individuals.map((_,0.0)))
			individuals.size
		}
		
		def createNewIndividuals(size:Int)(implicit currentGen:Int)
		
		def newIndividualFunc(p1:GeneticCode[I],p2:GeneticCode[I]):GeneticCode[I] = {
			val child = if(crossoverRate >= math.random) p1 crossover p2 else p1 crossover p1
			
			if(mutationRate >= math.random)
				child.mutate(mutationRatio, mutationSize)
			else
				child
		}
	}

}