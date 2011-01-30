package sweeney.nn.calc.combination

/**
 * Defines a function for combining neuron inputs. 
 */
trait InputCombinationFunction {
	
	protected def sum(inputs:Seq[Double]):Double = {
		inputs.reduceLeft(_ + _)
	}
	
	protected def average(inputs:Seq[Double]):Double = {
		sum(inputs) / inputs.size
	}
	
	protected def reduce(inputs:Seq[Double])(reduceOperation:(Seq[Double])=>Double):Double = {
		reduceOperation(inputs)
	}
	
	protected def splitAndCombine2Seq(inputs:Seq[Double],default:Double)
			(reduceOperation:(Seq[Double])=>Double)
			(combineOperation:(Double,Double)=>Double):Double = {
		val halfInd = inputs.size / 2
		
		if(halfInd == 0){
			default
		}
		else{
			val (firstHalf,secondHalf) = inputs.splitAt(halfInd)
			val first = reduce(firstHalf)(reduceOperation)
			val second = reduce(secondHalf)(reduceOperation)
			combineOperation(first,second)
		}
	}
	
	protected def splitAndCombine3Seq(inputs:Seq[Double],default:Double)
			(reduceOperation:(Seq[Double])=>Double)
			(combineOperation:(Double,Double,Double)=>Double):Double = {
		val thirdInd = inputs.size / 3
		
		if(thirdInd == 0){
			default
		}
		else{
			val (firstThird,secondAndThird) = inputs.splitAt(thirdInd)
			val (secondThird,thirdThird) = secondAndThird.splitAt(thirdInd)
			val first = reduce(firstThird)(reduceOperation)
			val second = reduce(secondThird)(reduceOperation)
			val third = reduce(thirdThird)(reduceOperation)
			combineOperation(first,second,third)
		}
	}

	def combineInputs(inputs:Seq[Double]):Double;

}
