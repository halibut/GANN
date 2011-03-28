package sweeney.nn

trait Testable[K] {
	self:NeuralNetwork[K] =>

	def getTestData:Seq[(Map[K,Double],Map[K,Double])];
	
	def getSingleTestError(expectedActualValues:Seq[(Double,Double)]):Double = {
		expectedActualValues.map{(ea) => 
			val d=ea._1-ea._2; 
			d*d
		}.reduceLeft(_+_)
	}
	
	def getAccumulatedTestError(singleTestErrors:Seq[Double]):Double = {
		singleTestErrors.reduceLeft(_+_)
	}
	
	def testNetwork():Double = {
		val testData = getTestData
		
		var errors = for(data <- testData) yield{
			val output = this.calculate(data._1)
			val expectedActuals = output.toSeq.map{keyVal =>
				val (key,actual) = keyVal
				(data._2(key), actual)
			}
			getSingleTestError(expectedActuals)
		}
		
		getAccumulatedTestError(errors)
	}
}