package sweeney.ga
package persistence

import scala.collection.immutable.IndexedSeq
import scala.collection.mutable.{ArrayBuffer,Map}

trait InMemoryPopulationPersistence[I] extends GAPopulationPersistenceProvider[I] {
	
	private lazy val _persistence = new InMemoryPopulationPersistence()
	def popPersistence:GAPopulationPersistence = _persistence;
	
	class InMemoryPopulationPersistence extends GAPopulationPersistence{
	
		private var _latestGeneration:Int = 0
		private var _populations:ArrayBuffer[Option[GenPopulation[I]]] = ArrayBuffer(None)
		
		def getLatestGeneration:Int = _latestGeneration
		def getPopulationSize(gen:Int):Option[Int] = {
			if(gen > _latestGeneration){
				None
			}
			else{
				_populations(gen).map(_.pop.size)	
			}
		}
	
		def getPopulation(gen:Int, from:Int=0, until:Int=(-1)):Option[Seq[(GeneticCode[I],Double)]] = {
			_populations(gen).map{pop =>
				var max = pop.pop.size
				if(until > 0)
					max = until
				pop.pop.slice(from,max)
			}
		}
		def getPopulation(gen:Int, indexes:Seq[Int]):Option[Seq[(GeneticCode[I],Double)]] = {
			_populations(gen).map{pop =>
				for(index <- indexes) yield {
					pop.pop(index)
				}
			}
		}
		def getIndividual(gen:Int, index:Int):Option[(GeneticCode[I],Double)] = {
			_populations(gen).map{pop =>
				pop.pop(index)
			}
		}
		def addOrUpdatePopulation(gen:Int, population:Seq[(GeneticCode[I],Double)]){
			val gens = getLatestGeneration
			if(gen > gens){
				_populations.appendAll(ArrayBuffer.fill(gen-gens)(None))
				_latestGeneration = gen
			}
			val pop = _populations(gen).getOrElse(new GenPopulation())
			_populations(gen) = Some(pop)
			
			pop.addOrUpdateFitMap(population)
		}
		def deletePopulation(gen:Int, from:Int=0){
			val gens = getLatestGeneration
			require(gen <= gens, "Cannot delete population because it doesn't exist.")
			val popOpt = _populations(gen)
			require(popOpt.isDefined, "Cannot delete population because it doesn't exist.")
			val pop = popOpt.get
			require(pop.popSize >= from, "From ("+from+") is not a valid index ("+pop.popSize+").")
			
			pop.remove(from, pop.popSize - from)
			if(pop.popSize == 0){
			    _populations(gen) = None
			}
		}
		
		def getTotalPopulationFitness(gen:Int):Option[Double] = {
			_populations(gen).map{pop =>
				pop.getTotalFitness
			}
		}
		
		private class GenPopulation[I]{
			private var _totalFitDirty = true;
			private var _totalFit:Double = 0.0
			private var _popDirty = true
			private val _pop:ArrayBuffer[(GeneticCode[I],Double)] = new ArrayBuffer()
			private val _fitMap:Map[GeneticCode[I],Double] = Map()
			
			def popSize = _fitMap.size
			
			def pop:IndexedSeq[(GeneticCode[I],Double)] = {
				if(_popDirty){
				    val sortedPop = _fitMap.toSeq.sortWith{(i1,i2)=>
						i2._2 < i1._2
					}
					_pop.clear
					_pop ++= sortedPop
					_popDirty = false
				}
				_pop.toIndexedSeq
			}
			
			def addOrUpdateFitMap(cfs:Seq[(GeneticCode[I],Double)]){
				_fitMap ++= cfs
				_popDirty = true
				_totalFitDirty = true
			}
			
			def remove(from:Int, to:Int){
				val toRemove = pop.slice(from,to).map(_._1)
				_fitMap --= toRemove
				_popDirty = true
				_totalFitDirty = true
			}
			
			def getTotalFitness:Double = {
				if(_totalFitDirty){
					_totalFit = _fitMap.values.reduceLeft(_+_)
					_totalFitDirty = false
				}
				_totalFit
			}
		}
	}
}