package sweeney.nn.calc.combination
import org.junit.{Test,Assert}

@Test
class MUXRCombinationTest {

	import Assert._
	
	val combineFunc = MUXCombination
	
	@Test
	def testFirst_3Inputs(){
		val c = combineFunc.combineInputs(Array(1.5, 3.5, 0))
		
		assertEquals(1.5, c, 0.00001)
	}
	
	@Test
	def testSecond_3Input(){
		val c = combineFunc.combineInputs(Array(1.5, 3.5, 1))
		
		assertEquals(3.5, c, 0.00001)
	}
	
	@Test
	def testFirst_7Inputs(){
		val c = combineFunc.combineInputs(Array(1.5,2.5, 3.5,4.5, 1,0,0))
		
		assertEquals(2, c, 0.00001)
	}
	
	@Test
	def testSecond_7Input(){
		val c = combineFunc.combineInputs(Array(1.5,2.5, 3.5,4.5, 1,0,1))
		
		assertEquals(4, c, 0.00001)
	}
	
	@Test
	def testFirst_8Inputs(){
		val c = combineFunc.combineInputs(Array(1.5,2.5, 3.5,4.5, 1,0,0,0))
		
		assertEquals(2, c, 0.00001)
	}
	
	@Test
	def testSecond_8Input(){
		val c = combineFunc.combineInputs(Array(1.5,2.5, 3.5,4.5, 1,0,1,1))
		
		assertEquals(4, c, 0.00001)
	}
}