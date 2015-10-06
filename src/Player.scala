import java.util.Scanner

/**
 * @author Tatsuya Oba
 */
class Player extends BaseAI{
  override val Name: String = "Player"

  override def getPutPosition(turn: Int, board: Board): Position = {
    val scanner = new Scanner(System.in)
    Position(scanner.nextInt, scanner.nextInt)
  }
}
