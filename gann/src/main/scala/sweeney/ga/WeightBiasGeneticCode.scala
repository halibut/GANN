package sweeney.ga

class WeightBiasGeneticCode(val weights:ChromosomeDouble,val biases:ChromosomeDouble) extends GeneticCode[WeightBiasGeneticCode]{
	
	override def fromIndividual(individual:WeightBiasGeneticCode):WeightBiasGeneticCode = {
		individual
	}
	
	override def mutate(mutationRatio:Double,mutationAmount:Double):WeightBiasGeneticCode = {
		val newWeights = weights.mutate(mutationRatio, mutationAmount)
		val newBiases = biases.mutate(mutationRatio, mutationAmount * .25)
		new WeightBiasGeneticCode(newWeights, newBiases)
	}

	override def crossover(mate:GeneticCode[WeightBiasGeneticCode]):WeightBiasGeneticCode = {
		val gcMate = mate.asInstanceOf[WeightBiasGeneticCode]
		val newWeights = weights.crossover(gcMate.weights).asInstanceOf[ChromosomeDouble]
		val newBiases = biases.crossover(gcMate.biases).asInstanceOf[ChromosomeDouble]
		new WeightBiasGeneticCode(newWeights, newBiases)
	}
	
	override def expressedIndividual:WeightBiasGeneticCode = {
		this
	}
}