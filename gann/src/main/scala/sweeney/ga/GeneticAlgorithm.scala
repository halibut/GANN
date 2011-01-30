package sweeney.ga

class GeneticAlgorithm[G,I](initialPopulation:Population[G,I],generation:Int = 0) {

	private var _population:Population[G,I] = initialPopulation
	private var _generation = generation
	
	def run(stopCondition:(Int,Population[G,I])=>Boolean):Population[G,I] = {
		
		while(!stopCondition(_generation,_population)){
			_population.testPopulation
			_population = _population.createNewGeneration
		}
		
		_population
	}
	
}