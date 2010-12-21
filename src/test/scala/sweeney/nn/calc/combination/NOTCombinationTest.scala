package sweeney.nn.calc.combination
import org.junit.{Test,Assert}

@Test
class NOTRCombinationTest {

	import Assert._
	
	val combineFunc = NOTCombination
	
	@Test
	def testFalse_1Input(){
		val c = combineFunc.combineInputs(Array(1))
		
		assertEquals(0, c, 0.00001)
	}
	
	@Test
	def testTrue_1Input(){
		val c = combineFunc.combineInputs(Array(0))
		
		assertEquals(1, c, 0.00001)
	}
	
	@Test
	def testFalse_NoInputs(){
		val c = combineFunc.combineInputs(Array[Double]())
		
		assertEquals(0, c, 0.00001)
	}
	
	@Test
	def testTrue_MultipleInputs(){
		val c = combineFunc.combineInputs(Array(0,1,0,0,1))
		
		assertEquals(1, c, 0.00001)
	}
	
	@Test
	def testFalse_MultipleInputs(){
		val c = combineFunc.combineInputs(Array(0,1,0,1,1))
		
		assertEquals(0, c, 0.00001)
	}
}