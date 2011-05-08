package sweeney.gann.example.tank

import sweeney.nn._
import sweeney.ga._
import sweeney.netimpl.genetic._
import game._
import network._

object TankGA {
	def main(args : Array[String]) : Unit = {
		val totalTargets = 35
		val totalMines = 10
		val targetAwareness = 4
		val mineAwareness = 4
		
		val targKeys:Seq[String] = (0 until targetAwareness).map{ i=>
					Seq[String]("targ"+i+"XDir", "targ"+i+"YDir", "targ"+i+"Dist", "targ"+i+"Exists")
				}.flatten.toSeq
		val mineKeys:Seq[String] = (0 until mineAwareness).map { i=>
					Seq[String]("mine"+i+"XDir", "mine"+i+"YDir", "mine"+i+"Dist", "mine"+i+"Exists")
				}.flatten.toSeq
		
		val inputKeys:Seq[String] = 
			Seq("posX","posY","lastLeft","lastRight","lastUp","lastDown") ++ targKeys ++ mineKeys
				
		val outputKeys = Seq("left","right","up","down");
		val hiddenLayers = Seq(inputKeys.size / 2, 15, 6)
		
		var gameBoards = (0 until 20).map(i=> new Board(numTargets=totalTargets,numMines=totalMines, size = 20))
		
		val gameTurns = 100
		val popSize = 1000
		val maxGen = 10000
		
		var resetMutAmt = 4.0
		var mutAmt = resetMutAmt
		var noChange = 0
		var lastFit = 0.0
		val origBoard = new Board(numTargets=totalTargets,numMines=totalMines, size = 20)
		
		val gann = new TankNetwork(inputKeys,outputKeys,hiddenLayers){
			
			override val populationSize = popSize
 			override def elitistPercentile = {if(noChange > 45) 0.0 else 0.02}
 			
 			override def mutationRatio:Double = {0.025}
 			override def mutationRate:Double = {0.25}
 			override def mutationSize:Double = { mutAmt }
 			override def crossoverRate:Double = { 0.7 }
 			
 			override def concurrentPages = 4
 			override def pageSize = math.min(10, popSize / concurrentPages)
			
			override def setupNetworkForIndividual(network:Perceptron[String],gc:WeightBiasGeneticCode){
				network.setBiases(gc.biases.geneSeq)
				network.setWeights(gc.weights.geneSeq)
			}
			
			override def calculateNetworkFitness(network:Perceptron[String]):Double ={
				var fitness = 0.0
				
				for(board <- gameBoards.map(_.clone())){
					fitness += playGame(network, board, gameTurns, targetAwareness, mineAwareness){(dx,dy)=>
					    Unit; //do nothing
					}
				}
				fitness;
			}
			
			//Stop when we reach a certain number of generations
			override def stopCondition():Boolean = {getGeneration >= maxGen}
			
			//Print results and create a randomized board between each generation
			override def afterGenerationTested(){
				val gen = getGeneration
 				val topFive = getPopulation(0,3)
 				val bottomFive = getPopulation(populationSize - 1)
 				println(gen+" -> "+topFive.map(_._2)+" ("+mutAmt+") "+" -> "+bottomFive.reverse.map(_._2))
 				
 				val best = topFive.head

 				mutAmt = mutAmt * .6
 				if((gen+1) % 20 == 0){
// 				    gameBoards = gameBoards.map{board =>
// 				        val newBoard = board.clone
// 				        newBoard.reset(true)
// 				        newBoard
// 				    }
 				    mutAmt = resetMutAmt
 				}
				
				val bestCode = best._1
				
				val network = createNetwork
			    setupNetworkForIndividual(network, bestCode.expressedIndividual)
				val fit = playGame(network, origBoard, 100, targetAwareness, mineAwareness){(x,y)=>}
				println("Best fitness = "+fit)
				if(fit > lastFit || gen == 1 || gen == maxGen){
				    lastFit = fit
				    println("================= "+gen+" =====================")
				    println("Improved to "+lastFit)
				    printGame(network, origBoard, 100, targetAwareness, mineAwareness)
				}
			}
		}
		
		//Chromosome is interlaced biases and weights, so initial population 
		//has to be the right length
		val initialPop = for(i <- 0 until popSize) yield {
			val wChrom = new ChromosomeDouble((0 until gann.weightsLength).map(i => 2.0 * math.random - 1.0).toIndexedSeq)
			val bChrom = new ChromosomeDouble((0 until gann.biasesLength).map(i => 0.2 * math.random - 0.1).toIndexedSeq)
			(new WeightBiasGeneticCode(wChrom,bChrom),0.0)
		}
		
		//Setup the genetic algorithm's initial population
		gann.initPopulation(initialPop,0)
		
		//Train the network
		val network = gann.trainNetwork()
		
		println(network)
	}
	
	def printGame(network:NeuralNetwork[String], gameBoard:Board, turns:Int, targetAwareness:Int, mineAwareness:Int):Double = {
		gameBoard.reset(false)
		println("BOARD - POSITIONS")
		println(gameBoard.size)
		println("TARGETS")
		for(target <- gameBoard.getClosestTargets){
		    println(target._1 + " " + target._2)
		}
		println("MINES")
		for(mine <- gameBoard.getClosestMines){
		    println(mine._1 + " " + mine._2)
		}
		println("MOVES")
		val fitness = playGame(network, gameBoard, turns, targetAwareness, mineAwareness){(dx,dy) =>
		    println(dx + " " + dy)
		}
		println("END")
		
		fitness
	}
	
	def getGameTurnInputs(lastLeft:Double,lastRight:Double,lastUp:Double,lastDown:Double,
	        boardState:Board,targetAwareness:Int, mineAwareness:Int):Map[String,Double] = {
		val tank = boardState.getTank
		val orientation = boardState.getOrientation
		val tankX = tank._1
		val tankY = tank._2
		var map = Map[String,Double](
			"posX" -> tankX,
			"posY" -> tankY,
			"lastLeft" -> lastLeft,
			"lastRight" -> lastRight,
			"lastUp" -> lastUp,
			"lastDown" -> lastDown)
	
		val targets = boardState.getClosestTargets
		var i = 0
		for(targ <- targets; if(i < targetAwareness)){
			map += "targ"+i+"Exists" -> 1.0
			val dx = targ._1 - tankX
			val dy = targ._2 - tankY
			val dist = math.sqrt(dx*dx + dy*dy)
			map += "targ"+i+"Dist" -> dist
			map += "targ"+i+"XDir" -> dx / dist
			map += "targ"+i+"YDir" -> dy / dist
			i+=1
		}
		while(i < targetAwareness){
			map += "targ"+i+"Exists" -> 0.0
			map += "targ"+i+"Dist" -> 0.0
			map += "targ"+i+"XDir" -> 0.0
			map += "targ"+i+"YDir" -> 0.0
			i+=1
		}
		
		val mines = boardState.getClosestMines
		i = 0
		for(mine <- mines; if(i < mineAwareness)){
			map += "mine"+i+"Exists" -> 1.0
			val dx = mine._1 - tankX
			val dy = mine._2 - tankY
			val dist = math.sqrt(dx*dx + dy*dy)
			map += "mine"+i+"Dist" -> dist
			map += "mine"+i+"XDir" -> dx / dist
			map += "mine"+i+"YDir" -> dy / dist
			i+=1
		}
		while(i < mineAwareness){
			map += "mine"+i+"Exists" -> 0.0
			map += "mine"+i+"Dist" -> 0.0
			map += "mine"+i+"XDir" -> 0.0
			map += "mine"+i+"YDir" -> 0.0
			i+=1
		}
		
		map
	}
	
	def playGame(network:NeuralNetwork[String],board:Board,
	        turns:Int,targetAwareness:Int,mineAwareness:Int)
			(onMoveFunc:(Double,Double)=>Unit):Double = {
	    board.reset(false)
		var i = 0
		var fitness = 0.0
		var lastUp = 0.0
		var lastDown = 0.0
		var lastRight = 0.0
		var lastLeft = 0.0
		var lastOrientation = board.getOrientation
		while(i < turns && board.getClosestTargets.size > 0){
		    val changeInOrientation = board.getOrientation - lastOrientation
		    lastOrientation = board.getOrientation
			val turnInputs:Map[String,Double] = getGameTurnInputs(lastLeft, lastRight, lastUp, lastDown, board, targetAwareness, mineAwareness);
			val outputs = network.calculate(turnInputs)
			val dx = (1.0 + outputs("right")) * 0.5 - (1.0 + outputs("left")) * 0.5
			val dy = (1.0 + outputs("up")) * 0.5 - (1.0 + outputs("down")) * 0.5
			lastLeft = outputs("left")
			lastRight = outputs("right")
			lastUp = outputs("up")
			lastDown = outputs("down")
			board.moveTank((dx,dy))
			fitness += board.getScore
			i+=1
			onMoveFunc(dx,dy)
		}
		fitness += (turns - i) * board.getScore
		fitness += board.getScore * turns * 10
		fitness
	}

	
}

