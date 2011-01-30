package sweeney.nn.calc.combination
import org.junit.{Test,Assert}

@Test
class CountCombinationTest {

	import Assert._
	
	val combineFunc = CountCombination
	
	@Test
	def testCount_1Input(){
		val c = combineFunc.combineInputs(Array(53))
		
		assertEquals(1, c, 0.00001)
	}
	
	@Test
	def testCount_2Inputs(){
		val c = combineFunc.combineInputs(Array(53, 45))
		
		assertEquals(2, c, 0.00001)
	}
	
	@Test
	def testCount_MultipleInputs(){
		val c = combineFunc.combineInputs(Array(53, 45, 6, 3, 45))
		
		assertEquals(5, c, 0.00001)
	}
	
	@Test
	def testCount_NoInputs(){
		val c = combineFunc.combineInputs(Array[Double]())
		
		assertEquals(0, c, 0.00001)
	}
	
}