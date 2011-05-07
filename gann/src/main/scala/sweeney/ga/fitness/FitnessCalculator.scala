package sweeney.ga
package fitness


trait FitnessCalculator[I] {

    def calculateFitness(individual:GeneticCode[I],existingFitness:Double):Double
}