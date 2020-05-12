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
        case _ => throw new IllegalCommandException
      }
  }

  def rotateRight(position: Position): Position = {
    position match {
      case facingNorth if facingNorth.direction == "N" =>
            Position(facingNorth.x, facingNorth.y, "W", facingNorth.palteauSize)
      case facingWest if facingWest.direction == "W" =>
            Position(facingWest.x, facingWest.y, "S", facingWest.palteauSize)
      case facingSouth if facingSouth.direction == "S" =>
            Position(facingSouth.x, facingSouth.y, "E", facingSouth.palteauSize)
      case facingEast if facingEast.direction == "E" =>
            Position(facingEast.x, facingEast.y, "N", facingEast.palteauSize)
    }
  }

  def rotateLeft(position: Position): Position = {
    position match {
      case facingNorth if facingNorth.direction == "N" =>
            Position(facingNorth.x, facingNorth.y, "E", facingNorth.palteauSize)
      case facingWest if facingWest.direction == "W" =>
            Position(facingWest.x, facingWest.y, "N", facingWest.palteauSize)
      case facingSouth if facingSouth.direction == "S" =>
            Position(facingSouth.x, facingSouth.y, "W", facingSouth.palteauSize)
      case facingEast if facingEast.direction == "E" =>
            Position(facingEast.x, facingEast.y, "S", facingEast.palteauSize)
    }
  }

  def moveForward(position: Position): Position = {
    position match {
      case facingNorth
        if facingNorth.direction == "N" => Position(
              facingNorth.x,
              facingNorth.y % facingNorth.palteauSize +1,
              "N",
              facingNorth.palteauSize)
      case facingWest
        if facingWest.direction == "W" => Position(
              facingWest.x % facingWest.palteauSize +1,
              facingWest.y,
              "W",
              facingWest.palteauSize)
      case facingSouth
        if facingSouth.direction == "S" => Position(
              facingSouth.x,
              if (facingSouth.y == 0) facingSouth.palteauSize -1 else facingSouth.y -1,
              "S",
              facingSouth.palteauSize)
      case facingEast
        if facingEast.direction == "E" => Position(
              if (facingEast.x == 0) facingEast.palteauSize -1 else facingEast.x -1,
              facingEast.y,
              "E",
              facingEast.palteauSize)
    }
  }
}
