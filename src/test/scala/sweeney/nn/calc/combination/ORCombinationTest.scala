package sweeney.nn.calc.combination
import org.junit.{Test,Assert}

@Test
class ORCombinationTest {

	import Assert._
	
	val combineFunc = ORCombination
	
	@Test
	def testTrue_2Inputs_11(){
		val c = combineFunc.combineInputs(Array(1,1))
		
		assertEquals(1, c, 0.00001)
	}
	
	@Test
	def testTrue_2Inputs_10(){
		val c = combineFunc.combineInputs(Array(1,0))
		
		assertEquals(1, c, 0.00001)
	}
	
	@Test
	def testTrue_2Inputs_01(){
		val c = combineFunc.combineInputs(Array(0,1))
		
		assertEquals(1, c, 0.00001)
	}
	
	@Test
	def testFalse_2Inputs_00(){
		val c = combineFunc.combineInputs(Array(0,0))
		
		assertEquals(0, c, 0.00001)
	}
	
	@Test
	def testFalse_NoInputs(){
		val c = combineFunc.combineInputs(Array[Double]())
		
		assertEquals(0, c, 0.00001)
	}
	
	@Test
	def testTrue_1Input(){
		val c = combineFunc.combineInputs(Array(1))
		
		assertEquals(1, c, 0.00001)
	}
	
	@Test
	def testFalse_1Input(){
		val c = combineFunc.combineInputs(Array(0))
		
		assertEquals(0, c, 0.00001)
	}
	
	@Test
	def testTrue_ManyInputs(){
		val c = combineFunc.combineInputs(Array(1,0,1,0,0))
		
		assertEquals(1, c, 0.00001)
	}
	
	@Test
	def testFalse_ManyInputs(){
		val c = combineFunc.combineInputs(Array(0,0,0,0,0,0))
		
		assertEquals(0, c, 0.00001)
	}
}