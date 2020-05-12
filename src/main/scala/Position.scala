case class Position(x: Int = 0, y: Int = 0, direction: String = "N", palteauSize: Int = 10) {
  override def toString: String = s"$x:$y:$direction"
}
