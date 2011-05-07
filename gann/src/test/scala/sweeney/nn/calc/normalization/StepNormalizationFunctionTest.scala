package sweeney.nn.calc.normalization
import org.junit.Test
import org.junit.Assert

@Test
class StepNormalizationFunctionTest {

	@Test
	def testMinStep{
		val n = StepNormalizationFunction.normalize(-1.5, -2, 2)
		
		Assert.assertEquals(0, n, 0.0000001)
	}
	
	@Test
	def testMidStep{
		val n = StepNormalizationFunction.normalize(0, -2, 2)
		
		Assert.assertEquals(1, n, 0.0000001)
	}
	
	@Test
	def testMaxStep{
		val n = StepNormalizationFunction.normalize(1.5, -2, 2)
		
		Assert.assertEquals(1, n, 0.0000001)
	}
	
	@Test
	def testDerivative{
		//step derivative is defined such that all the slope is 0 everywhere except
		//the step discontinuity (where it is infinit or undefined, but we're setting it to zero)
		
		(0 until 20).foreach{i=>
			val d = StepNormalizationFunction.derivative(math.random, 0, 1)
			Assert.assertEquals(0.0, d, 0.0000001)
		}
	}
}