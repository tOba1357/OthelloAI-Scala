/**
 * @author Tatasuya Oba
 */
class SimpleAI extends BaseAI {
  override val Name: String = "SimpleAI"

  override def getPutPosition(turn: Int, board: Board): Position = {
    for(x <- 1 to 8) {
      for(y <- 1 to 8) {
        if(board.putAble(Position(x, y), turn)) return {
          println(x, y)
          Position(x, y)
        }
      }
    }
    Position(0 ,0)
  }
}
