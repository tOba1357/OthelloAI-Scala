/**
 * @author Tatsuya Oba
 */
trait BaseAI {
  val Name: String
  def getPutPosition(turn: Int, board: Board): Position
}
