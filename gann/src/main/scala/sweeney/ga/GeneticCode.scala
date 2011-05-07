package sweeney.ga

/**
 * Trait that maps a sequence of Chromosomes to an actual instance of the object
 * @tparam I the type of the expressed object or individual
 */
trait GeneticCode[I] {
	
	/**
	 * @return the expression of the genetic code as an instantiated object.
	 */
	def expressedIndividual:I;
	
	/**
	 * Returns a new modified GeneticCode
	 * @param mutationRatio a proportion from 0 to 1 that specifies how much of the genetic
	 * material should be mutated.
	 * @param mutationSize a value that specifies the maximum difference between an individual 
	 * mutated gene and the original
	 * @return the new mutated GeneticCode
	 */
	def mutate(mutationRatio:Double,mutationSize:Double):GeneticCode[I];
	
	/**
	 * Returns a new modified GeneticCode based on crossover with the mate
	 * @param mate the GeneticCode of another individual that will be used in the crossover.
	 * @return the new mutated GeneticCode
	 */
	def crossover(mate:GeneticCode[I]):GeneticCode[I];
	
	
	def fromIndividual(individual:I):GeneticCode[I];

}