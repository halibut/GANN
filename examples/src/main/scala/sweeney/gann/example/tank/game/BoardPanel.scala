package sweeney.gann.example.tank.game

import java.awt.Dimension
import java.awt.RenderingHints
import scala.swing.event.ButtonClicked
import scala.swing.Button
import scala.swing.Orientation
import scala.swing.BoxPanel
import java.awt.Color
import java.awt.Graphics2D
import scala.swing.Component


class BoardPanel(gameBoard:Board, moves:Seq[(Double,Double)], movesAreTreads:Boolean) 
		extends BoxPanel(Orientation.Vertical) {
	
	private var _currentMove = 0;
	private val _maxMove = moves.size;
	private var _playing = false;
	private var _turnTimeMillis = 250;
	
	gameBoard.reset(false)
	private val _gameStates = (Seq((0.0,0.0))++moves).map{move =>
	    if(movesAreTreads)
	    	gameBoard.moveTank(move._1,move._2)
	    else
	        gameBoard.moveTank(move)
		GameState(
		    gameBoard.getScore, 
		    gameBoard.getTank,
		    gameBoard.getClosestTargets,
		    gameBoard.getCompletedTargets,
		    gameBoard.getClosestMines,
		    gameBoard.getExplodedMines)
	}.toIndexedSeq
	
	private val boardPane = this
	
	val startButton = new Button("|<")
	val prevButton = new Button("<")
	val playPauseButton = new Button("Play")
	val nextButton = new Button(">")
	val endButton = new Button(">|")
	
	boardPane.listenTo(startButton)
	boardPane.listenTo(prevButton)
	boardPane.listenTo(playPauseButton)
	boardPane.listenTo(nextButton)
	boardPane.listenTo(endButton)
  
	val board = new BoardComponent(gameBoard.size,gameBoard.radius){
	  preferredSize = new Dimension(400,400)
	}
	board.gameState = _gameStates(0)
	
	val buttons = new BoxPanel(Orientation.Horizontal){
		contents += startButton
		contents += prevButton
		contents += playPauseButton
		contents += nextButton
		contents += endButton
	}
	
	contents += board
	contents += buttons
	
	reactions += {
		case ButtonClicked(`playPauseButton`) => {
	  		if(_playing){
	  			playPauseButton.text = "Play"
		        setPlaying(false)
		    }
		    else if(_currentMove < _maxMove){
		    	playPauseButton.text = "Pause"
		    	setPlaying(true)
		    }
	  		buttons.repaint()
	  	}
		case ButtonClicked(button) => {
	  		setPlaying(false)
	  		_currentMove = button match {
	  		   case `startButton` => {0}
	  		   case `prevButton` => { math.max(_currentMove -1, 0) }
	  		   case `nextButton` => { math.min(_currentMove +1, _maxMove) }
	  		   case `endButton` => { _maxMove }
	  		}
	  		updateBoard()
		}
	}
	
	private def updateBoard(){
	    board.gameState = _gameStates(_currentMove)
	    board.repaint()
	}
	
	private def setPlaying(playGame:Boolean){
		if(!playGame){
			_playing = false
		}
		else{
		    if(_currentMove == _maxMove){
		        _playing = false
		    }
		    else{
		    	//Start a thread to run through the states until
		        //it is completed
		    	_playing = true
		    	new Thread{
		    		setDaemon(true);
		    	    override def run(){
		    	    	while(_playing && _currentMove < _maxMove){
		    	    		_currentMove += 1
		    	    		updateBoard()
		    	    		Thread.sleep(_turnTimeMillis)
		    	    	}
		    	    	playPauseButton.text = "Play"
		    	    	buttons.repaint()
		    	    }
		    	}.start()
		    }
		}
	}
}

case class GameState(
    val score:Double,
    val tank:(Double,Double),
    val targetsRemaining:Seq[(Double,Double)],
    val targetsFound:Seq[(Double,Double)],
    val minesRemaining:Seq[(Double,Double)],
    val minesExploded:Seq[(Double,Double)])

class BoardComponent(val boardSize:Double,val circleSize:Double) extends Component{
  
	var gameState:GameState = null
  
	override def paintComponent(g: Graphics2D):Unit = {
		super.paintComponent(g)
		g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON )
		
		if(gameState == null){
			return
		}
		
		//Determine size of mines/player based on the board size
		val width = this.bounds.width
		val height = this.bounds.height
		val dim:Int = math.min(width,height)
		val wOffset:Int = (width - dim) / 2
		val hOffset:Int = (height - dim) / 2
		
		val halfSize = boardSize / 2.0
		val boardScale = dim.asInstanceOf[Double] / boardSize
		val circleRad = (boardScale * circleSize).asInstanceOf[Int]
		
		//Convenience method for drawing a list of points
		def drawPoints(points:Seq[(Double,Double)])(drawFunc:(Int,Int)=>Unit){
			val movedPoints = points.map(point => 
			  ((point._1 + halfSize)*boardScale, (point._2 + halfSize)*boardScale))
			for(point <- movedPoints){
				val x:Int = point._1.asInstanceOf[Int] + wOffset
				val y:Int = point._2.asInstanceOf[Int] + hOffset
				drawFunc(x,y)
			}
		}
		
		//Draw the background
		g.setColor(BoardComponent.BOARD_COLOR)
		g.fillRect(wOffset,hOffset,dim,dim)
		
		//Draw the completed (found) targets
		g.setColor(BoardComponent.FOUND_TARGET_COLOR)
		drawPoints(gameState.targetsFound){(x,y)=>
		    drawStar(g,x,y,2,circleRad,circleRad)
		}
		
		//Draw the remaining targets
		val remainingTargets = gameState.targetsRemaining.splitAt(4)
		g.setColor(BoardComponent.CLOSE_TARGET_COLOR)
		drawPoints(remainingTargets._1)((x,y)=> drawCircle(g,x,y,circleRad) )
		g.setColor(BoardComponent.TARGET_COLOR)
		drawPoints(remainingTargets._2){(x,y)=> drawCircle(g,x,y,circleRad) }
		
		//Draw the exploded mines
		g.setColor(BoardComponent.EXPLODED_MINE_COLOR)
		drawPoints(gameState.minesExploded){(x,y)=>
		    drawStar(g,x,y,8,circleRad,circleRad / 2)
		}
		
		//Draw the remaining mines
		val remainingMines = gameState.minesRemaining.splitAt(4) 
		g.setColor(BoardComponent.CLOSE_MINE_COLOR)
		drawPoints(remainingMines._1){(x,y)=> drawCircle(g,x,y,circleRad) }
		g.setColor(BoardComponent.MINE_COLOR)
		drawPoints(remainingMines._2){(x,y)=> drawCircle(g,x,y,circleRad) }
		
		//Draw the Tank
		g.setColor(BoardComponent.TANK_COLOR)
		drawPoints(List[(Double,Double)](gameState.tank)){(x,y)=>
			drawCircle(g,x,y,circleRad)
		}
		
		//Draw the score
		g.setColor(BoardComponent.TEXT_COLOR)
		g.drawString("Score: "+gameState.score,wOffset + 20, hOffset + 20)

	}
	
	def drawCircle(g:Graphics2D, x:Int, y:Int, r:Int){
	    val r2 = 2 * r
	    g.fillOval(x-r,y-r,r2,r2)
	}
	
	def drawStar(g:Graphics2D, x:Int, y:Int, points:Int, rOuter:Int, rInner:Int){
		val radDivs =  math.Pi / points.asInstanceOf[Double]
	  	val xParts = new Array[Int](points * 2)
	  	val yParts = new Array[Int](points * 2)
	  	for(i <- 0 until points){
	  		xParts(i*2) = x + (math.cos(radDivs * 2 * i)*rOuter).asInstanceOf[Int]
	  		yParts(i*2) = y + (math.sin(radDivs * 2 * i)*rOuter).asInstanceOf[Int]
	  		xParts(i*2+1) = x + (math.cos(radDivs * (2 * i + 1))*rInner).asInstanceOf[Int]
	  		yParts(i*2+1) = y + (math.sin(radDivs * (2 * i + 1))*rInner).asInstanceOf[Int]
	  	}
	    g.fillPolygon(xParts,yParts,points * 2)
	}

}

object BoardComponent{
    val TARGET_COLOR = new Color(0.0f,0.8f,0.0f)
    val CLOSE_TARGET_COLOR = new Color(0.0f,0.6f,0.0f)
    val MINE_COLOR = new Color(0.8f,0.0f,0.0f)
    val CLOSE_MINE_COLOR = new Color(0.6f,0.0f,0.0f)
    val FOUND_TARGET_COLOR = new Color(0.5f,1.0f,0.5f)
    val EXPLODED_MINE_COLOR = new Color(1.0f,0.5f,0.0f)
    val TANK_COLOR = Color.BLACK
    val TEXT_COLOR = Color.BLACK
    val BOARD_COLOR = Color.LIGHT_GRAY
}