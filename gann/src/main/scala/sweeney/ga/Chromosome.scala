package sweeney.ga


/**
 * Trait for keeping track of individual chromosomes in a population.
 * @tparam G The type that represents the Genes in the chromosome. 
 * @tparam I The type representing an instance of an individual that
 * can be generated from this chromosome
 */
trait Chromosome[G,I] {

	protected var _chromosome:Seq[G] = null

	/**
	 * @return an instance of the individual represented by this chromosome
	 */
	def asIndividual:I

	/**
	 * @param individual is an instance of an individual represented by a chromosome
	 * Implementors of this method should transform from the individual instance and 
	 * create/update the _chromosome ArrayBuffer for this chromosome.
	 */
	protected def fromIndividual(individual:I):Unit
}