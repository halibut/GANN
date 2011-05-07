package sweeney.ga

import persistence._
import fitness._
import gen._

trait GeneticAlgorithm[I] {
	this: GAComponents[I] =>

	def initPopulation(pop:Seq[(GeneticCode[I],Double)], gen:Int = 0){
		popPersistence.addOrUpdatePopulation(gen,pop)
	}
	
	/**
	 * @return the size of each generation's population
	 */
	def populationSize:Int;
	
	/**
	 * After a new generation is created, the previous one will be cleaned up
	 * to save space. This property determines how many individuals in the previous 
	 * generation will be persisted. The default is zero.
	 * @return the number of previous generation individuals to perist
	 */
	def prevGenerationPopulationSize:Int = 0;
	
	/**
	 * @param from The start index of the population to return (default is 0)
	 * @param to The end index (exclusive) of the population to return (returns everything of left blank).
	 * @return the current population set specifying a start and end range
	 */
	def getPopulation(from:Int=0,to:Int=(-1)):Seq[(GeneticCode[I],Double)] = {
		popPersistence.getPopulation(popPersistence.getLatestGeneration,from,to).getOrElse(null)
	}
	
	/**
	 * @return the current generation
	 */
	def getGeneration():Int = {popPersistence.getLatestGeneration}
	
	/**
	 * Condition that tells the GeneticAlgorithm when it has produced a generation
	 * that satisfactorily solves the problem
	 * @return true if the GeneticAlgorithm should stop, false if it is not done yet
	 */
	def stopCondition():Boolean;

	/**
	 * User-defined logic that runs before the new generation is created
	 */
	def beforeNewGenerationCreated():Unit = {};
	
	/**
	 * User-defined logic that runs after the current generation is created
	 */
	def afterNewGenerationCreated():Unit = {};
	
	/**
	 * User-defined logic that runs before the current generation is tested
	 */
	def beforeGenerationTested():Unit = {};
	
	/**
	 * User-defined logic that runs after the current generation is tested
	 */
	def afterGenerationTested():Unit = {};
	
	/**
	 * Start running the Genetic Algorithm
	 */
	final def trainGenerations(){
		val initPop = getPopulation()
		if(initPop == null || initPop.size < populationSize)
			throw new IllegalStateException("Population must be initialized before training.")
		
		while(!stopCondition()){
			val curGen = popPersistence.getLatestGeneration
			
			//User defined function executes before the new generation is created
			beforeNewGenerationCreated()
			
			//Create the new generation
			newGenCreator.createNewGeneration(populationSize)
			val newGen = popPersistence.getLatestGeneration
			require(newGen > curGen, "Creating a new generation should have updated the total number of generations.")
			require(popPersistence.getPopulationSize(newGen).get == populationSize, "Expected "+populationSize+" population size, but got "+popPersistence.getPopulationSize(newGen).get)
			
			//delete some of the previous generation to make room
			popPersistence.deletePopulation(curGen,prevGenerationPopulationSize)
			
			//User defined function executes after the new generation is created
			afterNewGenerationCreated()
			
			//Run use-defined function before any testing takes place
			beforeGenerationTested()
			
			//Determine fitness of entire population
			fitnessTester.determinePopulationFitness
			
			//Run user-defined function after testing takes place
			afterGenerationTested()
		}
	}
}

trait GAComponents[I] extends GAPopulationPersistenceProvider[I] with 
		GAFitnessTestProvider[I] with GANewGenerationProvider[I] with
		GASelectionProvider[I]{
	this: GeneticAlgorithm[I] =>
}