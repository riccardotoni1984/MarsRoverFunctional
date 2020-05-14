object MarsRover {

  def execute(commands: String):String = {
    commands.foldLeft(Position())((p:Position, m: Char) => move(p,m)).toString
  }

  def execute(commands: String, plateauSize: Int):String = {
    commands.foldLeft(Position(palteauSize = plateauSize))((p:Position, m: Char) => move(p,m)).toString
  }

  def move(position: Position, move: Char): Position = {
      move match {
        case right if right == 'R' => rotateRight(position)
        case left if left == 'L' => rotateLeft(position)
        case move if move == 'M' => moveForward(position)
        case _ =>
          println(s"attempt to use illegal command($move) on position: ${position.toString}")
          position
      }
  }

  def rotateRight(position: Position): Position = {
    position match {
      case roverFacingNorth if roverFacingNorth.direction == "N" =>
            Position(roverFacingNorth.x, roverFacingNorth.y, "W", roverFacingNorth.palteauSize)
      case roverFacingWest if roverFacingWest.direction == "W" =>
            Position(roverFacingWest.x, roverFacingWest.y, "S", roverFacingWest.palteauSize)
      case roverFacingSouth if roverFacingSouth.direction == "S" =>
            Position(roverFacingSouth.x, roverFacingSouth.y, "E", roverFacingSouth.palteauSize)
      case roverFacingEast if roverFacingEast.direction == "E" =>
            Position(roverFacingEast.x, roverFacingEast.y, "N", roverFacingEast.palteauSize)
    }
  }

  def rotateLeft(position: Position): Position = {
    position match {
      case roverFacingNorth if roverFacingNorth.direction == "N" =>
            Position(roverFacingNorth.x, roverFacingNorth.y, "E", roverFacingNorth.palteauSize)
      case roverFacingWest if roverFacingWest.direction == "W" =>
            Position(roverFacingWest.x, roverFacingWest.y, "N", roverFacingWest.palteauSize)
      case roverFacingSouth if roverFacingSouth.direction == "S" =>
            Position(roverFacingSouth.x, roverFacingSouth.y, "W", roverFacingSouth.palteauSize)
      case roverFacingEast if roverFacingEast.direction == "E" =>
            Position(roverFacingEast.x, roverFacingEast.y, "S", roverFacingEast.palteauSize)
    }
  }

  def moveForward(position: Position): Position = {
    position match {
      case roverFacingNorth
        if roverFacingNorth.direction == "N" => Position(
              roverFacingNorth.x,
              roverFacingNorth.y % roverFacingNorth.palteauSize +1,
              "N",
              roverFacingNorth.palteauSize)
      case roverFacingWest
        if roverFacingWest.direction == "W" => Position(
              roverFacingWest.x % roverFacingWest.palteauSize +1,
              roverFacingWest.y,
              "W",
              roverFacingWest.palteauSize)
      case roverFacingSouth
        if roverFacingSouth.direction == "S" => Position(
              roverFacingSouth.x,
              if (roverFacingSouth.y == 0) roverFacingSouth.palteauSize -1 else roverFacingSouth.y -1,
              "S",
              roverFacingSouth.palteauSize)
      case roverFacingEast
        if roverFacingEast.direction == "E" => Position(
              if (roverFacingEast.x == 0) roverFacingEast.palteauSize -1 else roverFacingEast.x -1,
              roverFacingEast.y,
              "E",
              roverFacingEast.palteauSize)
    }
  }
}
