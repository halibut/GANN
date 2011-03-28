package sweeney.ga

trait GeneticAlgorithm[I] {

	val populationSize:Int;
	
	private var _currentPopulation:Seq[CodeFitness[I]] = null
	private var _currentGeneration:Int = 0
	
	def initPopulation(pop:Seq[CodeFitness[I]], gen:Int = 0){
		_currentPopulation = pop
		_currentGeneration = gen
	}
	
	/**
	 * @return true if population should be sorted such that the higher the fitness
	 * the better closer to the front of the list it will appear when sorted.
	 * false if a low fitness value is better.
	 */
	def highFitnessIsBetter = true
	
	/**
	 * @return The rate of genetic mutation when the current generation reproduces
	 */
	def mutationRate:Double = { 0.1 }
	
	/**
	 * @return The rate of genetic crossover when the current generation reproduces
	 */
	def crossoverRate:Double = { 0.7 }
	
	/**
	 * @return the percentile of the population that will be carried over to the new
	 * generation without modification. Should be in the range of 0 to 1
	 */
	def elitistPercentile = {0.01}
	
	def getPopulation() = {_currentPopulation}
	def getGeneration() = {_currentGeneration}
	
	/**
	 * Function that determines the fitness of the the given individual
	 * is toward solving the problem
	 * @return sequence of CodeFitness objects that map the 
	 * GeneticCode with the fitness value
	 */
	def determinePopulationFitness(currentPopulation:Seq[GeneticCode[I]]):Seq[CodeFitness[I]];
	
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
	
	private def createNewGeneration():Unit = {
		var newGen:Seq[GeneticCode[I]] = Seq()
		
		//Convert the population to an indexed seq because it will by faster
		//to search based on index
		val curpop = _currentPopulation.toIndexedSeq
		
		//Add unmodified elites to the new generation
		val elites = math.max(0,math.min(populationSize, (populationSize * elitistPercentile).asInstanceOf[Int]))
		if(_currentGeneration > 0 && elites > 0)
			newGen = newGen ++ curpop.slice(0, elites).map(_.code).distinct
		
		//Calculate the roulette sequence that will be used for selection of mates
		val roulette:IndexedSeq[Double] = if(_currentGeneration == 0){
			(0 until populationSize).map(_.asInstanceOf[Double]).toIndexedSeq
		}else{
			var tot = 0.0
			val rou = for(codeFitness <- curpop) yield {
				tot += codeFitness.fitness
				tot
			}
			rou.toIndexedSeq
		}
		
		val zippedRoulette = curpop.zip(roulette)
		val eliteSize = newGen.size
		val offspring = for(i <- 0 until (populationSize - eliteSize)) yield{
			val m1 = selectMate(zippedRoulette)
			val m2 = selectMate(zippedRoulette)
			val child = m1 crossover m2
			
			if(mutationRate > math.random)
				child.mutate(.5, math.random * mutationRate)
			else
				child
		}
		
		newGen = newGen ++ offspring

		_currentPopulation = newGen.map(CodeFitness(_,0))
		_currentGeneration += 1
	}

	private	def selectMate(roulette:IndexedSeq[(CodeFitness[I],Double)]):GeneticCode[I] = {
		val totalRoulette = roulette.last._2
		val rnd = totalRoulette * math.random

		val revRou = roulette.reverse
		val size = roulette.size
		var i = 0
		while(i < size && revRou(i)._2 > rnd){i+=1}
		
		revRou(i-1)._1 .code
	}
	
	/**
	 * Start running the Genetic Algorithm
	 */
	final def trainGenerations(){
		if(_currentPopulation == null || _currentPopulation.size != populationSize)
			throw new IllegalStateException("Population must be initialized before training.")
		
		while(!stopCondition()){
			//User defined function executes before the new generation is created
			beforeNewGenerationCreated()
			
			//Create the new generation
			createNewGeneration()
			
			//User defined function executes after the new generation is created
			afterNewGenerationCreated()
			
			//Run use-defined function before any testing takes place
			beforeGenerationTested()
			
			//Determine fitness of entire population
			val testedPop = determinePopulationFitness(_currentPopulation.map(_.code))
			require(testedPop.size == populationSize, "Expected "+populationSize+" population size, but got "+testedPop.size)
			_currentPopulation = testedPop
			
			//Sort the population such that more fit individuals are at
			//the front of the list
			_currentPopulation = _currentPopulation.sortWith{ (a,b) =>
				if(highFitnessIsBetter)	a.fitness > b.fitness else a.fitness < b.fitness }
			
			//Run user-defined function after testing takes place
			afterGenerationTested()
		}
	}
	
}