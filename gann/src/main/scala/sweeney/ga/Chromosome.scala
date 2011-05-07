package sweeney.ga


/**
 * Class for keeping track of individual chromosomes in an individual.
 * @tparam G The type that represents the Genes in the chromosome. 
 */
abstract class Chromosome[@specialized G](val geneSeq:IndexedSeq[G]) extends GeneticCode[IndexedSeq[G]]{
	def apply():IndexedSeq[G] = {
		geneSeq
	}
	
	override def expressedIndividual:IndexedSeq[G] = apply()
	
	override def fromIndividual(seq:IndexedSeq[G]):Chromosome[G];
	
	override def crossover(mate:GeneticCode[IndexedSeq[G]]):Chromosome[G] = {
		val chromMate = mate.asInstanceOf[Chromosome[G]]
		
		val max = geneSeq.size
		val (p1,p2) = (math.random,math.random)
		val (c1,c2) = if(p1 < p2) (p1,p2) else (p2,p1)
		val (i1,i2) = ((c1 * max).asInstanceOf[Int], (c2 * max).asInstanceOf[Int]) 
		val newSeq = for(i <- 0 until max) yield{
			if(i < i1 || i > i2)
				geneSeq(i)
			else
				chromMate.geneSeq(i)
		}
		
		this.fromIndividual(newSeq)
	}
	
	override def equals(other:Any):Boolean = {
		if(! other.isInstanceOf[this.type])
			false
		else
			other.asInstanceOf[this.type].geneSeq == geneSeq
	}
	
	override def hashCode:Int = {
		geneSeq.hashCode
	}
}

class ChromosomeDouble(geneSeq:IndexedSeq[Double]) extends Chromosome(geneSeq){
	
	override def fromIndividual(seq:IndexedSeq[Double]):ChromosomeDouble = {
		new ChromosomeDouble(seq)
	}

	override def mutate(mutationRatio:Double,mutationAmount:Double):ChromosomeDouble = {
		val newSeq = for(gene <- geneSeq) yield {
			if(math.random < mutationRatio)
				gene + math.random * mutationAmount - (2 * mutationAmount)
			else
				gene
		}
		
		this.fromIndividual(newSeq)
	}
}

class ChromosomeInt(geneSeq:IndexedSeq[Int]) extends Chromosome(geneSeq){
	
	override def fromIndividual(seq:IndexedSeq[Int]):ChromosomeInt = {
		new ChromosomeInt(seq)
	}

	override def mutate(mutationRatio:Double,mutationAmount:Double):ChromosomeInt = {
		val newSeq = for(gene <- geneSeq) yield {
			if(math.random < mutationRatio)
				gene + (math.random * mutationAmount - (2 * mutationAmount)).asInstanceOf[Int]
			else
				gene
		}
		
		this.fromIndividual(newSeq)
	}
}