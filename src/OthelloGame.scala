case class Direction(x: Int, y: Int)

case class Position(var x: Int, var y: Int) {
  def move(direction: Direction): Position = {
    x += direction.x
    y += direction.y
    this
  }

  def reverseMove(direction: Direction): Position = {
    x -= direction.x
    y -= direction.y
    this
  }

  def fillRange(): Boolean = {
    fillRange(x) && fillRange(y)
  }

  private def fillRange(num: Int): Boolean = {
   0 < num && num < 9
  }
}

class Board {
  val WALL = -1
  val BLANK = 0
  val BLACK = 1
  val WHITE = 2

  val directionList = List(Direction(1, 1), Direction(1, -1), Direction(1, 0)
    , Direction(-1, 1), Direction(-1, -1), Direction(-1, 0)
    , Direction(0, 1), Direction(0, -1)
  )
  val board = Array.ofDim[Int](10, 10)

  def setDefaultBoard(): Board = {
    for (i <- board.indices) {
      for (j <- board(i).indices) {
        (i, j) match {
          case (4, 4) | (5, 5) => board(i)(j) = BLACK
          case (4, 5) | (5, 4) => board(i)(j) = WHITE
          case (0, _) | (9, _) | (_, 0) | (_, 9) => board(i)(j) = WALL
          case _ => board(i)(j) = BLANK
        }
      }
    }
    this
  }

  def put(position: Position, color: Int): Boolean = {
    //not BLACK or WHITE
    if (color != WHITE && color != BLACK) {
      return false
    }
    if (getColor(position).getOrElse(false) != BLANK) {
      return false
    }
    val count = directionList.map(reverseLine(_, position.copy(), color)).sum
    if (count > 0) {
      changeColor(color, position)
      return true
    }
    false
  }

  def reverseLine(direction: Direction, position: Position, color: Int, count: Int = 0): Int = {
    val enemyColor = color ^ 3
    getColor(position.move(direction)).getOrElse(BLANK) match {
      case `color` =>
        (0 until count).foreach(_ => changeColor(color, position.reverseMove(direction)))
        count
      case `enemyColor` => reverseLine(direction, position, color, count + 1)
      case _ => 0
    }
  }

  def changeColor(color: Int, position: Position): Unit = {
    board(position.x)(position.y) = color
  }

  def getColor(position: Position): Option[Int] = {
    if (!position.fillRange()) {
      return None
    }
    Some(board(position.x)(position.y))
  }

  def show(): Unit = {
    board.foreach(line => {
      line.foreach {
        case BLACK => print("o")
        case WHITE => print("x")
        case BLANK => print(".")
        case _ =>
      }
      println()
    })
  }

  def putAbleLine(direction: Direction, position: Position, color: Int, count: Int = 0): Boolean = {
    val enemyColor = color ^ 3
    getColor(position.move(direction)).getOrElse(BLANK) match {
      case `color` =>
        if(count > 0){
          println(position ,direction)
        }
        count > 0
      case `enemyColor` => putAbleLine(direction, position, color, count + 1)
      case _ => false
    }
  }

  def putAble(position: Position, color: Int): Boolean = {
    directionList.exists(putAbleLine(_, position, color))
  }

  def putAblePositionCount(color: Int): Int = {
    (1 to 8).map(x => {
      (1 to 8).count(y => {
        putAble(Position(x, y), color)
      })
    }).sum
  }

  def getStoneNum(color: Int): Int = {
    (1 to 8).map(x => (1 to 8).count(y => getColor(Position(x, y)).getOrElse(BLANK) == color)).sum
  }

  def getWhiteStoneNum: Int = {
    getStoneNum(WHITE)
  }

  def getBlackStoneNum: Int = {
    getStoneNum(BLACK)
  }
}


object Board {
  def apply(): Board = {
    new Board
  }

  def default(): Board = {
    val board = new Board
    board.setDefaultBoard()
  }
}

class OthelloGame {
  val board = Board.default()
  var turn: Int = board.BLACK
  val BlackAI: BaseAI = new Player
  val WhiteAI: BaseAI = new Player

  def start(): Unit = {
    board.show()
    turn match {
      case board.BLACK =>
        println("turn:o")
        while (!board.put(BlackAI.getPutPosition(turn, board), turn)) {
          println("dont put")
          board.show()
        }
      case board.WHITE =>
        println("turn:x")
        while (!board.put(WhiteAI.getPutPosition(turn, board), turn)) {
          println("dont put")
          board.show()
        }
      case _ =>
        endGame()
        return
    }
    turn = setNextTurn()
    start()
  }

  def setNextTurn(): Int = {
    if (board.putAblePositionCount(turn ^ 3) > 0) {
      return turn ^ 3
    }
    if (board.putAblePositionCount(turn) > 0) {
      return turn
    }
    board.BLANK
  }

  def endGame(): Unit = {
    val whiteNum = board.getWhiteStoneNum
    val blackNum = board.getBlackStoneNum
    println(s"x:$whiteNum o:$blackNum")
    if (whiteNum == blackNum) {
      println("drew")
    } else if (blackNum > whiteNum) {
      println("black")
    } else {
      println("white")
    }
  }
}
