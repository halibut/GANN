package sweeney.gann.simple

import sweeney.ga._
import sweeney.nn._

import scala.math

/**
 * Trait that maps a Chromosomes to an actual instance of the object
 * @tparam I the type of the expressed object or individual
 */
class SimpleGeneticCode(val chromosome:Chromosome[Double], 
		val mutationRange:(Double,Double) = (-.5, 0.5)) extends GeneticCode[IndexedSeq[Double]] {
	
	require(mutationRange._1 <= mutationRange._2, "Mutation range should have lower number first, and higher number second. Found: "+mutationRange)
	private val _mutationMagnitude = mutationRange._2 - mutationRange._1
	private val _mutationMidpoint = _mutationMagnitude / 2
	
	/**
	 * @return the expression of the genetic code as an instantiated object.
	 */
	override def expressedIndividual:IndexedSeq[Double] = {
		chromosome.geneSeq
	}
	
	/**
	 * Returns a new modified GeneticCode
	 * @param mutationRatio a proportion from 0 to 1 that specifies how much of the genetic
	 * material should be mutated.
	 * @param mutationAmount a proportion from 0 to 1 that specifies how much each individual 
	 * mutated gene should differ from the original
	 * @return the new mutated GeneticCode
	 */
	override def mutate(mutationRatio:Double,mutationAmount:Double):SimpleGeneticCode = {
		val maxMagnitude = _mutationMagnitude * mutationAmount
		val halfMagnitude = maxMagnitude * 0.5
		val minMutation = _mutationMidpoint - halfMagnitude
		
		val newGeneSeq = chromosome.geneSeq.map{ gene =>
			if(mutationRatio >= math.random){
				val mutation = minMutation + math.random * maxMagnitude
				gene + mutation
			}
			else{
				gene
			}
		}
		
		new SimpleGeneticCode(new Chromosome(newGeneSeq),mutationRange)
	}
	
	/**
	 * Returns a new modified GeneticCode based on crossover with the mate
	 * @param mate the GeneticCode of another individual that will be used in the crossover.
	 * @return the new crossovered GeneticCode
	 */
	override def crossover(mate:GeneticCode[IndexedSeq[Double]]):SimpleGeneticCode = {
		val gSeq1 = chromosome.geneSeq
		val gSeq2 = chromosome.geneSeq
		
		val maxInd = chromosome.geneSeq.size
		val crossoverRange = Seq(math.random,math.random).sortWith(_ < _)
			.map(value => (maxInd * value).asInstanceOf[Int])
			
		val newGeneSeq = gSeq1.slice(0, crossoverRange(0)) ++ 
			gSeq2.slice(crossoverRange(0), crossoverRange(1)) ++
			gSeq1.slice(crossoverRange(1), maxInd) 
		
		new SimpleGeneticCode(new Chromosome(newGeneSeq),mutationRange)
	}
}