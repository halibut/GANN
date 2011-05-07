package sweeney.ga
package fitness

import persistence.GAPopulationPersistenceProvider
import sweeney.work.PagedWorkScheduler

trait PagedFitnessTest[I] extends GAFitnessTestProvider[I]{
	this: GAPopulationPersistenceProvider[I] =>
	
	private lazy val _fitTester = new PagedFitnessTester()
	
	def fitnessTester:GAFitnessTester = _fitTester
	def fitnessTestPageSize = 10
	def fitnessTestConcurrentPages = 4
	def calculatorPerIndividual = false
	final def calculatorPerPage = !calculatorPerIndividual

	def createFitnessCalculator():FitnessCalculator[I]
	
	class PagedFitnessTester extends GAFitnessTester{
				
		override final def determinePopulationFitness(){
			val gen = popPersistence.getLatestGeneration
			val popSize = popPersistence.getPopulationSize(gen).get
			
			new PagedWorkScheduler[Seq[(GeneticCode[I],Double)],Seq[(GeneticCode[I],Double)]]{
				override def pageSize = fitnessTestPageSize
				override def workSize = popSize
				override def concurrentPages = fitnessTestConcurrentPages
				
				override def pageSetup(start:Int,end:Int):Seq[(GeneticCode[I],Double)] = {
					popPersistence.getPopulation(gen,start,end).get
				}
				
				override def doPagedWork(pageData:Seq[(GeneticCode[I],Double)]):Seq[(GeneticCode[I],Double)] = {
					calculateFitness(pageData)
				}
				
				override def pageCombine(pageResults:Seq[Seq[(GeneticCode[I],Double)]]){
					popPersistence.addOrUpdatePopulation(gen,pageResults.flatten)
				}
			}.doWork()
		}
		
		def calculateFitness(population:Seq[(GeneticCode[I],Double)]):Seq[(GeneticCode[I],Double)] = {
			var calculator:FitnessCalculator[I] = null;
			if(calculatorPerPage)
			    calculator = createFitnessCalculator();
			for(indv <- population) yield {
				if(calculatorPerIndividual)
					calculator = createFitnessCalculator();
				
				val (gc,fit)= indv
				(gc, calculator.calculateFitness(gc,fit))
			}
		}
		
	}
}