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
	
	@Test
	def testDerivative{
		val d = normFunc.derivative(.5, 0, 1)
		Assert.assertEquals(1, d, 0.0000001)
	}
	
	@Test
	def testDerivative_min_max{
		val min = normFunc.derivative(0, 0, 1)
		val max = normFunc.derivative(1, 0, 1)
		Assert.assertEquals(1, min, 0.0000001)
		Assert.assertEquals(1, max, 0.0000001)
	}
	
	@Test
	def testDerivative_outsideRange{
		val min = normFunc.derivative(-0.01, 0, 1)
		val max = normFunc.derivative(1.01, 0, 1)
		Assert.assertEquals(0, min, 0.0000001)
		Assert.assertEquals(0, max, 0.0000001)
	}
	
	@Test
	def testDerivative_shifted{
		//input range is from -1 to 1, output is from 1 to 5
		//meaning slope of the line is 2
		val d = normFunc.derivative(.5, -1, 1, 1, 5)
		Assert.assertEquals(2, d, 0.0000001)
	}
	
	@Test
	def testDerivative_min_max_shifted{
		val min = normFunc.derivative(-1, -1, 1, 1, 5)
		val max = normFunc.derivative(1, -1, 1, 1, 5)
		Assert.assertEquals(2, min, 0.0000001)
		Assert.assertEquals(2, max, 0.0000001)
	}
	
	@Test
	def testDerivative_outsideRange_shifted{
		val min = normFunc.derivative(-1.01, -1, 1, 1, 5)
		val max = normFunc.derivative(1.01, -1, 1, 1, 5)
		Assert.assertEquals(0, min, 0.0000001)
		Assert.assertEquals(0, max, 0.0000001)
	}
}