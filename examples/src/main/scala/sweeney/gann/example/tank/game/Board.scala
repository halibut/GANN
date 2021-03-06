package sweeney.gann.example.tank.game

import scala.collection.mutable.ArrayBuffer

class Board(val numTargets:Int, val numMines:Int = 0, val size:Double=20.0) {
	type Vec = (Double,Double)
	type Point = (Double,Double)
	type Line = (Double,Double,Double)
	
	require(numTargets > 0, "There must be at least 1 target. Found:"+numTargets)
	require(numMines >= 0, "Number of mines must be positive. Found:"+numMines)
	
	val mineScoreDec = 5
	val targetScoreInc = 3
	
	val radius = 0.5
	val maxX = size/2.0
	val minX = -maxX
	val maxY = size/2.0
	val minY = -maxY
	
	private var _score:Int = 0
	private var _tank:Point = (0.0,0.0)
	private var _orientation:Double = 0.0
	private var _targets:ArrayBuffer[Point] = null
	private var _remainingTargetIndexes:Seq[Int] = null
	private var _remainingMineIndexes:Seq[Int] = null
	reset(true)
	
	def this(targets:Seq[(Double,Double)],mines:Seq[(Double,Double)],size:Double){
		this(targets.size,mines.size,size)
		
		//default constructor does a reset, so all we need to do
		//is set the positions of the targets and mines
		_targets = ArrayBuffer((targets ++ mines):_*)
	}
	
	def getClosestTargets():Seq[Point] = {
		val remainTargs = _remainingTargetIndexes.map{i=>_targets(i)}
		return getClosests(remainTargs)
	}
	
	def getClosestMines():Seq[Point] = {
		val remainMines = _remainingMineIndexes.map{i=>_targets(i)}
		return getClosests(remainMines)
	}
	
	def getCompletedTargets():Seq[Point] = {
		val completedInds = (0 until numTargets).filter(!_remainingTargetIndexes.contains(_))
		val completedTargets = completedInds.map((i)=>_targets(i))
		return getClosests(completedTargets)
	}
	
	def getExplodedMines():Seq[Point] = {
		val explodedMineInds = (numTargets until numTargets+numMines).filter(!_remainingMineIndexes.contains(_))
		val explodedMine = explodedMineInds.map((i)=>_targets(i))
		return getClosests(explodedMine)
	}
	
	def getTank = _tank
	def getScore = _score
	def getOrientation = _orientation
	
	def moveTank(moveVec:Vec):Unit = {
		if(moveVec == (0.0,0.0))
			return;
		
		val mag = dist(moveVec,(0.0,0.0))
		val vec = if(mag > 1.0)
			normalize(moveVec)
		else
			moveVec
			
		updateGameWithNewPos(vec._1,vec._2)
	}
	
	def moveTank(leftTread:Double,rightTread:Double):Unit = {
		if(leftTread == 0.0 && rightTread == 0.0)
			return;
		
		val rNorm = math.min(1.0, math.max(-1.0, rightTread))
		val lNorm = math.min(1.0, math.max(-1.0, leftTread))
		
		val diff = rightTread - leftTread;
		val diffNorm = (rNorm - lNorm) / 2.0
		
		val moveDist = 
		    if(diffNorm > 0.0) (1.0 - diffNorm)
			else (-1.0 - diffNorm) 
		
		_orientation = _orientation + (math.Pi * diffNorm * 0.5)	
		
		val dPos = {
			(math.cos(_orientation) * moveDist,
			math.sin(_orientation) * moveDist)
		}
		
		updateGameWithNewPos(dPos._1,dPos._2)
	}
	
	private def updateGameWithNewPos(dx:Double,dy:Double){
	    val newPos = {
			var tmpX = _tank._1 + dx
			var tmpY = _tank._2 + dy
			if(tmpX > maxX - radius) tmpX = maxX - radius;
			if(tmpX < minX + radius) tmpX = minX + radius;
			if(tmpY > maxY - radius) tmpY = maxY - radius;
			if(tmpY < minY + radius) tmpY = minY + radius;
			(tmpX, tmpY)
		}
		
		val (targetsDestroyed,minesHit) = updateTargets(_tank,newPos)
		
		_tank = newPos
		_score += targetsDestroyed * targetScoreInc - minesHit * mineScoreDec
	}
	
	def reset(randomizeTargets:Boolean = false){
		if(randomizeTargets)
			_targets = randomTargets()
			
		_remainingTargetIndexes = (0 until numTargets).toSeq
		_remainingMineIndexes = (numTargets until numTargets+numMines).toSeq
		_tank = (0.0,0.0)
		_score = numMines * mineScoreDec
	}
	
	override def clone():Board = {
		val newBoard = new Board(numTargets, numMines, size)
		newBoard._score = this._score
		newBoard._tank = this._tank
		newBoard._targets = ArrayBuffer(this._targets:_*)
		newBoard._remainingTargetIndexes = Seq(this._remainingTargetIndexes:_*)
		newBoard._remainingMineIndexes = Seq(this._remainingMineIndexes:_*)
		newBoard
	}
	
	private def updateTargets(curPos:Point,newPos:Point):(Int,Int) = {
		val origTargets = _remainingTargetIndexes.size
		val origMines = _remainingMineIndexes.size
		
		val directContacts = 
			for(i <- (_remainingTargetIndexes ++ _remainingMineIndexes);
				if(overlap(newPos,_targets(i)))) yield {i}
		
		_remainingTargetIndexes = _remainingTargetIndexes.diff(directContacts)
		_remainingMineIndexes = _remainingMineIndexes.diff(directContacts)
		
//		val pathBorder = getPathBorder(curPos,newPos)
//		
//		val pathContacts = 
//			for(i <- (_remainingTargetIndexes ++ _remainingMineIndexes);
//				if(overlapPolygon(_targets(i),pathBorder))) yield {i}
//		
//		_remainingTargetIndexes = _remainingTargetIndexes.diff(pathContacts)
//		_remainingMineIndexes = _remainingMineIndexes.diff(pathContacts)
		
		(origTargets - _remainingTargetIndexes.size, origMines - _remainingMineIndexes.size)
	}

	private def getClosests(points:Seq[Point]):Seq[Point] = {
		val remainTargs = points.map{p=>
			val tDist = dist(p,_tank)
			(p,tDist)
		}
		remainTargs.sortWith((t1,t2) => (t1._2 < t2._2)).map(_._1)
	}
	private def dist(c1:Point,c2:Point):Double = {
		val dx = c2._1 - c1._1
		val dy = c2._2 - c1._2
		return math.sqrt(dx*dx + dy*dy)
	}
	private def dist(p:Point,l:Line):Double = {
		dot(p,(l._1,l._2)) + l._3
	}
	private def overlap(c1:Point,c2:Point):Boolean = {
		val pDist = dist(c1,c2)
		return (pDist <= 2.0 * radius)
	}
	private def overlapPolygon(p:Point,polygon:Seq[Line]):Boolean = {
		polygon.forall(dist(p,_) <= 0.0)
	}
	private def normalize(v:Vec):Vec = {
		val mag = dist((0.0,0.0),v)
		if(mag == 0.0)
			return (0.0,0.0)
		return mult(v, 1/mag)
	}
	private def mult(v:Vec,s:Double):Vec = {
		(v._1 * s, v._2 * s)
	}
	private def add(p1:Point,p2:Point):Point = {
		(p1._1 + p2._1, p1._2 + p2._2)
	}
	private def dot(v1:Vec,v2:Vec):Double = {
		v1._1 * v2._1 + v1._2 * v2._2
	}
	private def vec(c1:Point,c2:Point):Vec = {
		(c2._1 - c1._1, c2._2 - c1._2)
	}
	private def line(p:Point,v:Vec):Line = {
		val d = -dot(p,v)
		(v._1,v._2,d)
	}
	private def getPathBorder(curPos:Point,newPos:Point):Seq[Line] = {
		val rad2 = radius * 2
		val vector = vec(curPos,newPos)
		val nVector = normalize(vector)
		val tangent = (-nVector._2,nVector._1)
		val p1 = add(curPos, mult(nVector,rad2))
		val p2 = add(newPos, mult(nVector,-rad2))
		Seq[Line](
			line(p1, mult(nVector,-1)),
			line(p1, tangent),
			line(p2, nVector),
			line(p2, mult(tangent,-1)))
	}
	
	private def randomTargets():ArrayBuffer[Point] = {
		val min = (minX + radius, minY + radius)
		val mul = (size - radius * 2, size - radius * 2)
		var i = 0
		var targs = new ArrayBuffer[Point](numTargets+numMines)
		while(i < numTargets+numMines){
			val avoidTargs = Seq(_tank) ++ targs.slice(0,i)
			val testPoint = (min._1 + mul._1 * math.random, min._2 + mul._2 * math.random)
			if(i == 0 || avoidTargs.forall(!overlap(_,testPoint))){
				targs 
				targs += testPoint
				i+=1
			}
		}
		targs
	}
}