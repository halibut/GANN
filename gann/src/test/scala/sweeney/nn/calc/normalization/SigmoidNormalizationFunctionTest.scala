package sweeney.nn.calc.normalization
import org.junit.Test
import org.junit.Assert

@Test
class SigmoidNormalizationFunctionTest {
	
	val normFunc = SigmoidNormalizationFunction

	@Test
	def testNegativeOutsideMin{
		val n = normFunc.normalize(-6, -2, 2)
		
		Assert.assertTrue(.001 < n && n < .1)
	}
	
	@Test
	def testPositiveOutsideMax{
		val n = normFunc.normalize(6, -2, 2)
		
		Assert.assertTrue(.9 < n && n < .999)
	}
	
	@Test
	def testZero{
		val n = normFunc.normalize(0, -1, 1)
		
		Assert.assertEquals(.5, n, 0.001)
	}
	
	@Test
	def testZero_Shifted{
		val n = normFunc.normalize(-2, -4, 0, 1, 3)
		
		Assert.assertEquals(2, n, 0.001)
	}
	
	@Test
	def testNegativeInsideMin{
		val n = normFunc.normalize(-.01, -1, 1)
		
		Assert.assertTrue(.4 < n && n < .499)
	}
	
	@Test
	def testPositiveInsideMax{
		val n = normFunc.normalize(.01, -1, 1)
		
		Assert.assertTrue(.501 < n && n < .6)
	}
}