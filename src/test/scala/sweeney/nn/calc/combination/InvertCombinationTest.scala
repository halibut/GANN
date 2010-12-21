package sweeney.nn.calc.combination
import org.junit.{Test,Assert}

@Test
class SumCombinationTest {

	import Assert._
	
	val combineFunc = SumCombination

	@Test
	def testSum_1Input(){
		val c = combineFunc.combineInputs(Array(7))
		
		assertEquals(7.0, c, 0.00001)
	}
	
	@Test
	def testSum_2Inputs(){
		val c = combineFunc.combineInputs(Array(1,3))
		
		assertEquals(4.0, c, 0.00001)
	}
	
	@Test
	def testSum_MultipleInputs(){
		val c = combineFunc.combineInputs(Array(1,3,5,7,9))
		
		assertEquals(25, c, 0.00001)
	}
	
	@Test
	def testSum_NoInputs(){
		val c = combineFunc.combineInputs(Array[Double]())
		
		assertEquals(0, c, 0.00001)
	}
	
	
}