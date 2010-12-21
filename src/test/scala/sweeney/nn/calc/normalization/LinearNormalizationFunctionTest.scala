package sweeney.nn.calc.normalization
import org.junit.Test
import org.junit.Assert

@Test
class LinearNormalizationFunctionTest {
	
	val normFunc = LinearNormalizationFunction

	@Test
	def testMin{
		val n = normFunc.normalize(-3, -2, 2)
		
		Assert.assertEquals(0, n, 0.0000001)
	}
	
	@Test
	def testMax{
		val n = normFunc.normalize(3, -2, 2)
		
		Assert.assertEquals(1, n, 0.0000001)
	}
	
	@Test
	def testMid{
		val n = normFunc.normalize(0, -2, 2)
		
		Assert.assertEquals(.5, n, 0.0000001)
	}
	
	@Test
	def testMid_Shifted{
		val n = normFunc.normalize(-2, -4, 0, 1, 3)
		
		Assert.assertEquals(2, n, 0.0000001)
	}
	
	@Test
	def testNegativeInRange{
		val n = normFunc.normalize(-1, -2, 2)
		
		Assert.assertEquals(.25, n, 0.0000001)
	}
	
	@Test
	def testPositiveInRange{
		val n = normFunc.normalize(1, -2, 2)
		
		Assert.assertEquals(.75, n, 0.0000001)
	}
	
	@Test
	def testNegativeInRange_Shifted{
		val n = normFunc.normalize(-3, -4, 0, 1, 3)
		
		Assert.assertEquals(1.5, n, 0.0000001)
	}
	
	@Test
	def testPositiveInRange_Shifted{
		val n = normFunc.normalize(-1, -4, 0, 1,3)
		
		Assert.assertEquals(2.5, n, 0.0000001)
	}
}