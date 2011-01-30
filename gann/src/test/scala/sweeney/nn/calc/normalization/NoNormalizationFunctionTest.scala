package sweeney.nn.calc.normalization
import org.junit.Test
import org.junit.Assert

@Test
class NoNormalizationFunctionTest {

	val normFunc = NoNormalizationFunction
	
	@Test
	def testNoNormalization{
		
		for(i <- 0 until 10){
			val rand = math.random * 20 - 10
			
			val n = normFunc.normalize(rand, 0, 0, 0)
			Assert.assertEquals(rand, n, 0.0000001)
		}
		
	}
	
}