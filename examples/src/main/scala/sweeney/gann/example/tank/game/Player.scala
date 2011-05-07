package sweeney.gann.example.tank.game

import scala.collection.mutable.ArrayBuffer
import scala.swing.MainFrame
import scala.swing.SimpleSwingApplication

object Player extends SimpleSwingApplication {

	def top = new MainFrame {
		title = "Tank Game Visualizer"
		
		val (board,moves,treads) = getGameFromStdIn
		
		contents = new BoardPanel(board, moves, treads)
	}
	
	override def startup(args: Array[String]) {
	    super.startup(args)
	}
	
	private def getGameFromStdIn:(Board,Seq[(Double,Double)],Boolean) = {
	    var str = readLine
	    require(str.contains("BOARD"),"First line of input should start with \"BOARD\".")
	    val treads = str.contains("TREAD")
	    
	    str = readLine
	    val boardSize = str.replaceAll("\\s*,*\\s","").toDouble
	    
	    require(readLine.contains("TARGETS"),"Could not find beginning of \"TARGETS\" section.")
	    
	    str = readLine
	    val targets = ArrayBuffer[(Double,Double)]()
	    while(!str.contains("MINES")){
	        //There should be 2 double values on each line separated by whitespace
	        val posStrs = str.split("\\s*,*\\s")
	        require(posStrs.size == 2, "Target lines must have 2 values")
	        targets += ((posStrs(0).toDouble, posStrs(1).toDouble))
	        str = readLine
	    }
	    
	    str = readLine
	    val mines = ArrayBuffer[(Double,Double)]()
	    while(!str.contains("MOVES")){
	        //There should be 2 double values on each line separated by whitespace
	        val posStrs = str.split("\\s*,*\\s")
	        require(posStrs.size == 2, "Mine lines must have 2 values")
	        mines += ((posStrs(0).toDouble, posStrs(1).toDouble))
	        str = readLine
	    }
	        
	    str = readLine
	    val moves = ArrayBuffer[(Double,Double)]()
	    while(!str.contains("END")){
	        //There should be 2 double values on each line separated by whitespace
	        val posStrs = str.split("\\s*,*\\s")
	        require(posStrs.size == 2, "Move lines must have 2 values")
	        moves += ((posStrs(0).toDouble, posStrs(1).toDouble))
	        str = readLine
	    }
	    
	    val board = new Board(targets, mines, boardSize)
	    (board,moves,treads)
	}
}
