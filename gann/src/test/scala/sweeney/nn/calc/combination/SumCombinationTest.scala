package sweeney.nn.calc.combination
import org.junit.{Test,Assert}

@Test
class InvertCombinationTest {

	import Assert._
	
	val combineFunc = InvertCombination

	@Test
	def testInvert_1Input(){
		val c = combineFunc.combineInputs(Array(7))
		
		assertEquals(1.0 / 7.0, c, 0.00001)
	}
	
	@Test
	def testInvert_2Inputs(){
		val c = combineFunc.combineInputs(Array(1,3))
		
		assertEquals(1.0 / 2.0, c, 0.00001)
	}
	
	@Test
	def testInvert_MultipleInputs(){
		val c = combineFunc.combineInputs(Array(1,3,5,7,9))
		
		assertEquals(1.0 / 5.0, c, 0.00001)
	}
	
	@Test
	def testInvert_NoInputs(){
		val c = combineFunc.combineInputs(Array[Double]())
		
		assertEquals(0, c, 0.00001)
	}
	
	
}