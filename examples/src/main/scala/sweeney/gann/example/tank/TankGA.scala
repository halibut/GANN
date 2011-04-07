package sweeney.gann.example.tank

import sweeney.nn._
import sweeney.ga._
import sweeney.netimpl.genetic._
import sweeney.netimpl.genetic.simple._
import game._
import network._

object TankGA {
	def main(args : Array[String]) : Unit = {
		val totalTargets = 15
		val totalMines = 5
		val targetAwareness = 1
		val mineAwareness = 1
		
		val targKeys:Seq[String] = (0 until targetAwareness).map{ i=>
					Seq[String]("targ"+i+"X", "targ"+i+"Y", "targ"+i+"Exists")
				}.flatten.toSeq
		val mineKeys:Seq[String] = (0 until mineAwareness).map { i=>
					Seq[String]("mine"+i+"X", "mine"+i+"Y", "mine"+i+"Exists")
				}.flatten.toSeq
		
		val inputKeys:Seq[String] = 
			Seq("posX","posY") ++ targKeys ++ mineKeys
				
				
		val outputKeys = Seq("dirX","dirY");
		val hiddenLayers = Seq(8,3)
		
		val gameBoard = new Board(numTargets=totalTargets,numMines=totalMines, width=20, height = 20)
		val gameTurns = 500
		val gameScoreMult = 100.0
		val popSize = 50
		val maxGen = 1000
		
		val network = new TankNetwork(inputKeys,outputKeys,hiddenLayers){
			
			override val populationSize = popSize
 			override def elitistPercentile = 0.1
			
			override def setupNetworkForIndividual(gc:WeightBiasGeneticCode){
				setBiases(gc.biases.geneSeq)
				setWeights(gc.weights.geneSeq)
			}
			
			override def determinePopulationFitness(currentPopulation:Seq[GeneticCode[WeightBiasGeneticCode]]):Seq[CodeFitness[WeightBiasGeneticCode]] = {
				for(gc <- currentPopulation) yield {
					setupNetworkForIndividual(gc.expressedIndividual)
					gameBoard.reset()
					
					//println("Starting test...")
					var i = 0
					var fitness = 0
					while(i < gameTurns && gameBoard.getClosestTargets.size > 0){
						val inputs = getGameTurnInputs(gameBoard, targetAwareness, mineAwareness)
						val outputs = calculate(inputs)
						val move = (outputs("dirX"),outputs("dirY"))
						gameBoard.moveTank(move)
						fitness += gameBoard.getScore
						i+=1
						//println("    turn"+i);
					}
					//println("... finished test.")
					
					CodeFitness(gc,fitness + gameBoard.getScore * (gameTurns - i))
				}
			}
			
			//Stop when we reach a certain number of generations
			override def stopCondition():Boolean = {getGeneration >= maxGen}
			
			
			//Print results and create a randomized board between each generation
			override def afterGenerationTested(){
				val gen = getGeneration
 				val pop = getPopulation
 				println(gen+" -> "+pop.slice(0,5).map(_.fitness)+" -> "+pop.reverse.slice(0,5).map(_.fitness))
				
				if(gen % 5 == 0){
					setupNetworkForIndividual(pop.head.code.expressedIndividual)
					gameBoard.reset()
					val sb = new StringBuffer()
					
					var i = 0
					var fitness = 0
					while(i < gameTurns && gameBoard.getClosestTargets.size > 0){
						val inputs = getGameTurnInputs(gameBoard, targetAwareness, mineAwareness)
						val outputs = calculate(inputs)
						val move = (outputs("dirX"),outputs("dirY"))
						gameBoard.moveTank(move)
						sb.append(gameBoard.getTank).append(", ")
						i+=1
						//println("    turn"+i);
					}
					
					println("turns("+i+") - moves("+sb.toString+")")
				}
				gameBoard.reset(true) 
			}
		}
		
		//Chromosome is interlaced biases and weights, so initial population 
		//has to be the right length
		val initialPopGC = for(i <- 0 until popSize) yield {
			val wChrom = new ChromosomeDouble((0 until network.weightsLength).map(i => 4 * math.random - 2.0).toIndexedSeq)
			val bChrom = new ChromosomeDouble((0 until network.biasesLength).map(i => 2 * math.random - 1.0).toIndexedSeq)
			new WeightBiasGeneticCode(wChrom,bChrom)
		}
		val initialPop = initialPopGC.map(gc => new CodeFitness(gc,0.0))
		
		//Setup the genetic algorithm's initial population
		network.initPopulation(initialPop,0)
		
		//Train the network
		network.trainNetwork()
	}
	

	private def getGameTurnInputs(boardState:Board, targetAwareness:Int, mineAwareness:Int):Map[String,Double] = {
		var map = Map[String,Double](
			"posX" -> boardState.getTank._1,
			"posY" -> boardState.getTank._2)
	
		val targets = boardState.getClosestTargets
		var i = 0
		for(targ <- targets; if(i < targetAwareness)){
			map += "targ"+i+"X" -> targ._1
			map += "targ"+i+"Y" -> targ._2
			map += "targ"+i+"Exists" -> 1.0
			i+=1
		}
		while(i < targetAwareness){
			map += "targ"+i+"X" -> 0.0
			map += "targ"+i+"Y" -> 0.0
			map += "targ"+i+"Exists" -> 0.0
			i+=1
		}
		
		val mines = boardState.getClosestMines
		i = 0
		for(targ <- targets; if(i < mineAwareness)){
			map += "mine"+i+"X" -> targ._1
			map += "mine"+i+"Y" -> targ._2
			map += "mine"+i+"Exists" -> 1.0
			i+=1
		}
		while(i < mineAwareness){
			map += "mine"+i+"X" -> 0.0
			map += "mine"+i+"Y" -> 0.0
			map += "mine"+i+"Exists" -> 0.0
			i+=1
		}
		
		map
	}
}

