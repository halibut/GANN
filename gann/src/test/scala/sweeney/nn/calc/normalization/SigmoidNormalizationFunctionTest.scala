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
	
	def derivCalc(value:Double, minInput:Double, maxInput:Double, minOutput:Double = 0, maxOutput:Double = 1):Double = {
		val stepSize = (maxInput - minInput) / 100000.0
		
		val f1 = normFunc.normalize(value - stepSize, minInput, maxInput, minOutput, maxOutput)
		val f2 = normFunc.normalize(value + stepSize, minInput, maxInput, minOutput, maxOutput)
		
		(f2 - f1) / (stepSize * 2)
	}
	
	@Test
	def testDerivative{
		val test = derivCalc(.5,0,1)
		val d = normFunc.derivative(.5, 0, 1)
		Assert.assertEquals(test, d, 0.0000001)
	}
	
	@Test
	def testDerivative_min_max{
		val testMin = derivCalc(0, 0, 1)
		val testMax = derivCalc(1, 0, 1)
		val min = normFunc.derivative(0, 0, 1)
		val max = normFunc.derivative(1, 0, 1)
		Assert.assertEquals(testMin, min, 0.0000001)
		Assert.assertEquals(testMax, max, 0.0000001)
	}
	
	@Test
	def testDerivative_outsideRange{
		val testMin = derivCalc(-0.01, 0, 1)
		val testMax = derivCalc(1.01, 0, 1)
		val min = normFunc.derivative(-0.01, 0, 1)
		val max = normFunc.derivative(1.01, 0, 1)
		Assert.assertEquals(testMin, min, 0.0000001)
		Assert.assertEquals(testMax, max, 0.0000001)
	}
	
	@Test
	def testDerivative_shifted{
		//input range is from -1 to 1, output is from 1 to 5
		//meaning slope of the line in the middle is roughly 4 / 2 = 2
		val test = derivCalc(0, -1, 1, 1, 5)
		val d = normFunc.derivative(0, -1, 1, 1, 5)
		Assert.assertEquals(test, d, 0.0000001)
	}
	
	@Test
	def testDerivative_min_max_shifted{
		val testMin = derivCalc(-1, -1, 1, 1, 5)
		val testMax = derivCalc(1, -1, 1, 1, 5)
		val min = normFunc.derivative(-1, -1, 1, 1, 5)
		val max = normFunc.derivative(1, -1, 1, 1, 5)
		Assert.assertEquals(testMin, min, 0.0000001)
		Assert.assertEquals(testMax, max, 0.0000001)
	}
	
	@Test
	def testDerivative_outsideRange_shifted{
		val testMin = derivCalc(-1.01, -1, 1, 1, 5)
		val testMax = derivCalc(1.01, -1, 1, 1, 5)
		val min = normFunc.derivative(-1.01, -1, 1, 1, 5)
		val max = normFunc.derivative(1.01, -1, 1, 1, 5)
		Assert.assertEquals(testMin, min, 0.0000001)
		Assert.assertEquals(testMax, max, 0.0000001)
	}
	
	@Test
	def testDerivative_randomTests{
		(0 until 100).foreach{ i =>
			val input = math.random * 50 - 25
			var inRange = (math.random * 50 - 25, math.random * 50 - 25)
			if(inRange._1 > inRange._2)
				inRange = (inRange._2,inRange._1)
			var outRange = (math.random * 50 - 25, math.random * 50 - 25)
			if(outRange._1 > outRange._2)
				outRange = (outRange._2,outRange._1)
			val test = derivCalc(input,inRange._1,inRange._2,outRange._1,outRange._2)
			val d = normFunc.derivative(input,inRange._1,inRange._2,outRange._1,outRange._2)
			Assert.assertEquals(test, d, 0.0000001)
		}
	}
}