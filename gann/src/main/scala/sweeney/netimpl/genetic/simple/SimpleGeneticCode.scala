package sweeney.netimpl.genetic.simple

import sweeney.ga._
import sweeney.nn._

import scala.math

/**
 * Trait that maps a Chromosomes to an actual instance of the object
 * @tparam I the type of the expressed object or individual
 */
class SimpleGeneticCode(val chromosome:IndexedSeq[Double]) extends GeneticCode[IndexedSeq[Double]] {
	
	/**
	 * @return the expression of the genetic code as an instantiated object.
	 */
	override def expressedIndividual:IndexedSeq[Double] = {
		chromosome
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
		val minMutation = - mutationAmount / 2
		
		val newGeneSeq = chromosome.map{ gene =>
			if(mutationRatio >= math.random){
				val mutation = minMutation + math.random * mutationAmount
				gene + mutation
			}
			else{
				gene
			}
		}
		
		new SimpleGeneticCode(newGeneSeq)
	}
	
	/**
	 * Returns a new modified GeneticCode based on crossover with the mate
	 * @param mate the GeneticCode of another individual that will be used in the crossover.
	 * @return the new crossovered GeneticCode
	 */
	override def crossover(mate:GeneticCode[IndexedSeq[Double]]):SimpleGeneticCode = {
		val gSeq1 = chromosome
		val gSeq2 = mate.expressedIndividual
		
		val maxInd = chromosome.size
		val crossoverRange = Seq(math.random,math.random).sortWith(_ < _)
			.map(value => (maxInd * value).asInstanceOf[Int])
			
		val newGeneSeq = gSeq1.slice(0, crossoverRange(0)) ++ 
			gSeq2.slice(crossoverRange(0), crossoverRange(1)) ++
			gSeq1.slice(crossoverRange(1), maxInd) 
		
		new SimpleGeneticCode(newGeneSeq)
	}
	
	override def equals(other:Any):Boolean = {
		other match{
			case o:SimpleGeneticCode => o.chromosome == chromosome
			case _ => false
		}
	}
	
	override def hashCode:Int = {
		chromosome.hashCode
	}
}