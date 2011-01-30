package sweeney.nn.calc.combination
import org.junit.{Test,Assert}

@Test
class AddCombinationTest {

	import Assert._
	
	val combineFunc = AddCombination
	
	@Test
	def testSubtract_2Inputs(){
		val c = combineFunc.combineInputs(Array(1,3))
		
		assertEquals(1.0 + 3.0, c, 0.00001)
	}
	
	@Test
	def testSubtract_3Inputs(){
		val c = combineFunc.combineInputs(Array(1,3,5))
		
		assertEquals(1.0 + 4.0, c, 0.00001)
	}
	
	@Test
	def testSubtract_4Inputs(){
		val c = combineFunc.combineInputs(Array(1,3,5,7))
		
		assertEquals(2.0 + 6.0, c, 0.00001)
	}
	
	@Test
	def testSubtract_NoInputs(){
		val c = combineFunc.combineInputs(Array[Double]())
		
		assertEquals(0, c, 0.00001)
	}

	@Test
	def testSubtract_1Input(){
		val c = combineFunc.combineInputs(Array(7))
		
		assertEquals(0, c, 0.00001)
	}
	
}