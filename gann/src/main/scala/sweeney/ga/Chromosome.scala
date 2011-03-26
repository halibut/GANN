package sweeney.ga


/**
 * Class for keeping track of individual chromosomes in an individual.
 * @tparam G The type that represents the Genes in the chromosome. 
 */
class Chromosome[+G](val geneSeq:IndexedSeq[G]) {
	def apply():IndexedSeq[G] = {
		geneSeq
	}
}