package sweeney.ga

abstract class GeneticAlgorithm[I](_initialPopulation:Seq[GeneticCode[I]],_initialGeneration:Int = 0) {

	val populationSize = _initialPopulation.size
	
	/**
	 * @return true if population should be sorted such that the higher the fitness
	 * the better closer to the front of the list it will appear when sorted.
	 * false if a low fitness value is better.
	 */
	def highFitnessIsBetter = true
	
	/**
	 * @return The rate of genetic mutation when the current generation reproduces
	 */
	def mutationRate:Double = { 0.01 }
	
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
	 * Function that determines how "fit" the given individual
	 * is toward solving the problem
	 * @param individual the expressed GeneSeq
	 * @return the fitness value
	 */
	def determineFitness(individual:I):Double;
	
	/**
	 * Condition that tells the GeneticAlgorithm when it has produced a generation
	 * that satisfactorily solves the problem
	 * @return true if the GeneticAlgorithm should stop, false if it is not done yet
	 */
	def stopCondition():Boolean;

	/**
	 * User-defined logic that runs before the current generation is tested
	 */
	def beforeGenerationTesting(currentPopulation:Seq[GeneticCode[I]],currentGeneration:Int):Unit = {};
	
	/**
	 * User-defined logic that runs after the current generation is tested
	 */
	def afterGenerationTesting(currentPopulationAndFitness:Seq[(GeneticCode[I],Double)],currentGeneration:Int):Unit = {};
	
	private def createNewGeneration(currentPopulationAndFitness:Seq[(GeneticCode[I],Double)]):Seq[GeneticCode[I]] = {
		//Sort the population such that more fit individuals are at
		//the front of the list
		val sortedPopulation = currentPopulationAndFitness.sortWith{ (a,b) =>
			if(highFitnessIsBetter)
				a._2 > b._2
			else
				a._2 < b._2
		}
		
		val elites = math.max(0,math.min(populationSize, (populationSize * elitistPercentile).asInstanceOf[Int]))
		
		var newGen:Seq[GeneticCode[I]] = Seq()
		
		//TODO add the required new Individuals to this generation's population
		
		newGen
	}
	
	/**
	 * Start running the Genetic Algorithm
	 */
	final def run(){
		
		var currentPopulation = _initialPopulation
		var currentGeneration = _initialGeneration 
		
		while(!stopCondition()){
			//Run use-defined function before any testing takes place
			beforeGenerationTesting(currentPopulation, currentGeneration)
			
			//Determine fitness of entire population
			val currentPopAndFitness = for(geneSeq <- currentPopulation) yield {
				val individual = geneSeq.expressedIndividual
				val fitness = determineFitness(individual)
				(geneSeq,fitness)
			}
			
			//Run user-defined function after testing takes place
			afterGenerationTesting(currentPopAndFitness, currentGeneration)

			//Create new population from the old one
			currentPopulation = createNewGeneration(currentPopAndFitness)
		}
	}
	
}