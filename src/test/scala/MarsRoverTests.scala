import org.scalatest.FlatSpec

class MarsRoverTests extends FlatSpec{

  behavior of "Mars Rover"

  it should "turn right" in{
    assert(MarsRover.execute("R") == "0:0:W")
    assert(MarsRover.execute("RR") == "0:0:S")
    assert(MarsRover.execute("RRR") == "0:0:E")
    assert(MarsRover.execute("RRRR") == "0:0:N")
  }

  it should "turn left" in{
    assert(MarsRover.execute("L") == "0:0:E")
    assert(MarsRover.execute("LL") == "0:0:S")
    assert(MarsRover.execute("LLL") == "0:0:W")
    assert(MarsRover.execute("LLLL") == "0:0:N")
  }

  it should "move forward" in{
    assert(MarsRover.execute("M") == "0:1:N")
    assert(MarsRover.execute("MM") == "0:2:N")
  }

  it should "move forward after rotation" in{
    assert(MarsRover.execute("RM") == "1:0:W")
    assert(MarsRover.execute("RMM") == "2:0:W")
  }

  it should "wrap-around when at the end of the grid" in{
    assert(MarsRover.execute("LM") == "9:0:E")
    assert(MarsRover.execute("LLMM") == "0:8:S")
    assert(MarsRover.execute("LLLMMMMMMMMMMM") == "1:0:W")
    assert(MarsRover.execute("MMMMMMMMMMMMMM") == "0:4:N")
  }

  it should "return an error if command string contains illegal character" in{
    intercept[IllegalCommandException]{
      MarsRover.execute("LF")
    }
  }
}
