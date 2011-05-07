package sweeney.netimpl.genetic.testing
import sweeney.netimpl.genetic.GANN
import sweeney.nn.NeuralNetwork

trait ErrorBasedTesting[I, NK, N <: NeuralNetwork[NK]] {
this:GANN[I, NK, N] =>
    	def getTestData:Seq[(Map[NK,Double],Map[NK,Double])];	 	override def calculateNetworkFitness(network:N):Double = { 		val testData = getTestData;				var errors = for(data <- testData) yield{			val output = network.calculate(data._1);			val expectedActuals = output.toSeq.map{keyVal =>				val (key,actual) = keyVal;				(data._2(key), actual);			};			getSingleTestError(expectedActuals);		};				1.0 / getAccumulatedTestError(errors); 	}; 	def getSingleTestError(expectedActualValues:Seq[(Double,Double)]):Double = {		expectedActualValues.map{(ea) => 			val d=ea._1-ea._2; 			d*d;		}.reduceLeft(_+_);	};		def getAccumulatedTestError(singleTestErrors:Seq[Double]):Double = {		singleTestErrors.reduceLeft(_+_)	}
}