package sweeney.ga
package gen

import persistence.GAPopulationPersistenceProvider

import scala.annotation.tailrec


trait RouletteSelection[I] extends GASelectionProvider[I]{
	this: GAPopulationPersistenceProvider[I] =>
	
	def getPopulationSelector(generation:Int):GAPopulationSelector = 
		new RoulettePopulationSelector(generation)

	class RoulettePopulationSelector(gen:Int) extends GAPopulationSelector{
		private val _totalFit:Double = popPersistence.getTotalPopulationFitness(gen).get
		var curAdjFit = 0.0
		private val _popRoulette = for(gcf <- popPersistence.getPopulation(gen).get) yield {
			val fitness = gcf._2
			curAdjFit += (fitness / _totalFit)
			curAdjFit
		}
		
		private val _first = (0, _popRoulette.head)
		private val _last = (_popRoulette.size-1, _popRoulette.last)
		
		def selectMates(numMates:Int):Seq[GeneticCode[I]] = {
			val indexes = for(i <- 0 until numMates) yield {
				val pos = math.random
				val ind = findIndividual(_first,_last,pos)
				ind
			}
			
			popPersistence.getPopulation(gen, indexes).get.map(_._1)
		}
		
		@tailrec
		private def findIndividual(start:(Int,Double),end:(Int,Double),find:Double):Int = {
			if(start._1 == end._1){
				return start._1
			}
			else if(start._1 + 1 == end._1){
				if(start._1 > find)
					return start._1
				else
					return end._1	
			}
			
			val midInd = (end._1 - start._1) / 2 + start._1
			val midFit = _popRoulette(midInd)
			
			if(find < midFit)
				return findIndividual(start, (midInd,midFit), find)
			else
				return findIndividual((midInd,midFit), end, find)
		}
	}

}