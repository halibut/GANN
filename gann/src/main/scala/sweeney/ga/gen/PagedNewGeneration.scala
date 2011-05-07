package sweeney.ga
package gen

import persistence.GAPopulationPersistenceProvider
import sweeney.work.PagedWorkScheduler

trait PagedNewGenerationProvider[I] extends GANewGenerationProvider[I] {
this: GAPopulationPersistenceProvider[I] with 
	GeneticAlgorithm[I] with GASelectionProvider[I]=>

	private lazy val _creator = new PagedNewGenerationCreator()
	def newGenCreator:GANewGenerationCreator = _creator;

	def genCreatorPageSize = 10;
	def genCreatorConcurrentPages = 4;
	
	class PagedNewGenerationCreator extends GANewGenerationCreator{
		
		def createNewIndividuals(size:Int)(implicit currentGen:Int) = {
			val newGen = currentGen + 1
			val selector = getPopulationSelector(currentGen)
			
			new PagedWorkScheduler[Int,Seq[(GeneticCode[I],Double)]]{
				override def pageSize = genCreatorPageSize
				override def concurrentPages = genCreatorConcurrentPages
				override def workSize = size
				
				override def pageSetup(start:Int,end:Int):Int = {
					end - start
				}
				
				override def doPagedWork(pageSize:Int):Seq[(GeneticCode[I],Double)] = {
				    val p1Mates = selector.selectMates(pageSize)
					val p2Mates = selector.selectMates(pageSize)
					
					p1Mates.zip(p2Mates).map{(parents) =>
						(newIndividualFunc(parents._1,parents._2),0.0)
					}
				}
				
				override def pageCombine(pageResults:Seq[Seq[(GeneticCode[I],Double)]]){
					val combinedPages = pageResults.flatten
					popPersistence.addOrUpdatePopulation(newGen, combinedPages)
				}
			}.doWork()
		}
	}

}