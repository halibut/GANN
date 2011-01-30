package sweeney.nn.calc.combination
import org.junit.{Test,Assert}

@Test
class AverageCombinationTest {

	import Assert._
	
	val combineFunc = AverageCombination
	
	@Test
	def testAvg_2Inputs(){
		val c = combineFunc.combineInputs(Array(1,3))
		
		assertEquals(2, c, 0.00001)
	}
	
	@Test
	def testAvg_1Input(){
		val c = combineFunc.combineInputs(Array(7))
		
		assertEquals(7, c, 0.00001)
	}
	
	@Test
	def testAvg_MultipleInputs(){
		val c = combineFunc.combineInputs(Array(1,3,5,7,9))
		
		assertEquals(5, c, 0.00001)
	}
	
	@Test
	def testAvg_NoInputs(){
		val c = combineFunc.combineInputs(Array[Double]())
		
		assertEquals(0, c, 0.00001)
	}
	
	
}