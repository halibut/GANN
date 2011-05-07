package sweeney.work
import scala.collection.mutable.ArrayBuffer

import scala.actors.Future
import scala.actors.Futures._

trait PagedWorkScheduler[S,C] extends GAWorkScheduler {

	def doWork() = {
		require(pageSize > 0, "pageSize must be > 0, found:"+pageSize)
		require(workSize >= 0, "workSize must be a positive integer, found:"+workSize)
		require(concurrentPages >= 0, "concurrentPages must be a positive integer, found:"+concurrentPages)
		
		val partialLastPage:Int = workSize % pageSize
		val pages:Int = workSize / pageSize + (if(partialLastPage != 0) 1 else 0)
		
		var startInd:Int = 0
		val parPages:ArrayBuffer[S] = new ArrayBuffer() 
		for(i <- 0 until pages){
			
			val endInd:Int = if(i < pages -1 || partialLastPage == 0){
				startInd + pageSize
			}
			else{
				startInd + partialLastPage
			}
			
			//Create a new thread to start doing work on this page
			parPages +=	pageSetup(startInd,endInd)
			
			startInd = endInd
			
			//If we have reached the max number of concurrent
			//pages, then wait for all to finish
			if(parPages.size >= concurrentPages){
			    val parFutures:Seq[Future[C]] = parPages.map{ pageData =>
			      	future{doPagedWork(pageData)}
			    }
				val finishedFutures = parFutures.map((fut) => fut.apply())
				pageCombine(finishedFutures)
				parPages.clear
			}
		}
		
		//wait for any remaining concurrent pages to finish
		val parFutures:Seq[Future[C]] = parPages.map{ pageData =>
	      	future{doPagedWork(pageData)}
	    }
		val finishedFutures = parFutures.map((fut) => fut.apply())
		pageCombine(finishedFutures)
		parPages.clear
		
	}
	
	def pageSize:Int;
	def workSize:Int;
	def concurrentPages:Int = 1
	
	/**
	 * Implement this method to perform some calculation asynchronously
	 */
	def doPagedWork(page:S):C
	
	/**
	 * Implement this method to synchronously set up data to be used 
	 * later in a parallel calculation
	 */
	def pageSetup(start:Int,end:Int):S
	
	/**
	 * Implement this method to synchronously combine multiple pages
	 * (calculations) back together
	 */
	def pageCombine(pageResults:Seq[C]):Unit
}