package com.itv

import org.scalatest._

import scala.util.Try

class MarsRoverSpec extends WordSpecLike with Matchers {

  /**
    * Default UnitVector points up by default: (0, 1)
    * 0 in the x direction, +1 in the y direction
    */

  "A Unit Vector" should {

    "rotate clockwise by 90 degrees when rotating once" in {

      val unitVector = UnitVector()
      unitVector.rotateClockwise shouldBe UnitVector(1,0)

    }

    "rotate clockwise by 180 degrees when rotating twice" in {

      val unitVector = UnitVector()
      unitVector.rotateClockwise.rotateClockwise shouldBe UnitVector(0,-1)

    }

    "rotate clockwise by 270 degrees when rotating three times" in {

      val unitVector = UnitVector()
      unitVector.rotateClockwise.rotateClockwise.rotateClockwise shouldBe UnitVector(-1,0)

    }

    "rotate clockwise by 360 degrees when rotating four times" in {

      val unitVector = UnitVector()
      unitVector.rotateClockwise.rotateClockwise.rotateClockwise.rotateClockwise shouldBe unitVector

    }

    "rotate anti-clockwise by 90 degrees when rotating once" in {

      val unitVector = UnitVector()
      unitVector.rotateAntiClockwise shouldBe UnitVector(-1,0)

    }

    "rotate anti-clockwise by 180 degrees when rotating twice" in {

      val unitVector = UnitVector()
      unitVector.rotateAntiClockwise.rotateAntiClockwise shouldBe UnitVector(0,-1)

    }

    "rotate anti-clockwise by 270 degrees when rotating three times" in {

      val unitVector = UnitVector()
      unitVector.rotateAntiClockwise.rotateAntiClockwise.rotateAntiClockwise shouldBe UnitVector(1,0)

    }

    "rotate anti-clockwise by 360 degrees when rotating four times" in {

      val unitVector = UnitVector()
      unitVector.rotateAntiClockwise.rotateAntiClockwise.rotateAntiClockwise.rotateAntiClockwise shouldBe unitVector

    }

    "not allow a length larger than 1 or smaller than -1" in {

      val unitVector1 = Try(UnitVector(-2, 0)).toOption
      val unitVector2 = Try(UnitVector(2, 0)).toOption
      val unitVector3 = Try(UnitVector(0, 2)).toOption
      val unitVector4 = Try(UnitVector(0, 2)).toOption

      assert(unitVector1.isEmpty)
      assert(unitVector2.isEmpty)
      assert(unitVector3.isEmpty)
      assert(unitVector4.isEmpty)

    }

  }

  "A Point" should {

    "not allow negative values for x and y" in {

      val point1 = Try(Point(-1, 0)).toOption
      val point2 = Try(Point(0, -1)).toOption

      assert(point1.isEmpty)
      assert(point2.isEmpty)


    }

  }

  "A MarsRover" should {

    "not move when rotating clockwise/anti-clockwise from its given position" in {

      val marsRover = MarsRover(Point(0,0), Array(Array(0,0,0), Array(0,0,0), Array(0,0,0)))

      val startPosition = marsRover.getCurrentPosition

      marsRover.rotateClockwise

      assert(startPosition == marsRover.getCurrentPosition)

      marsRover.rotateAntiClowise

      assert(startPosition == marsRover.getCurrentPosition)

    }

    "move up" when {

      "from starting position" in {
        val marsRover = MarsRover(Point(1,1), Array(Array(0,0,0), Array(0,0,0), Array(0,0,0)))

        marsRover.moveForward
        assert(marsRover.getCurrentPosition == Point(1,2))
      }

    }

    "move right" when {

      "in starting position and one clockwise rotation" in {
        val marsRover = MarsRover(Point(1,1), Array(Array(0,0,0), Array(0,0,0), Array(0,0,0)))

        marsRover.rotateClockwise
        marsRover.moveForward
        assert(marsRover.getCurrentPosition == Point(2,1))
      }

    }

    "move down" when {

      "in starting position and two clockwise rotations" in {
        val marsRover = MarsRover(Point(1,1), Array(Array(0,0,0), Array(0,0,0), Array(0,0,0)))

        marsRover.rotateClockwise
        marsRover.rotateClockwise
        marsRover.moveForward
        assert(marsRover.getCurrentPosition == Point(1,0))
      }

    }

    "move left" when {

      "in starting position and three clockwise rotations" in {
        val marsRover = MarsRover(Point(1,1), Array(Array(0,0,0), Array(0,0,0), Array(0,0,0)))

        marsRover.rotateClockwise
        marsRover.rotateClockwise
        marsRover.rotateClockwise
        marsRover.moveForward
        assert(marsRover.getCurrentPosition == Point(0,1))
      }

    }

    "move from (x, max y) to (x, min y) - when on the edge and vector pointing up" in {

      val marsRover = MarsRover(Point(0,2), Array(Array(0,0,0), Array(0,0,0), Array(0,0,0)))

      // MarsRover default position is up - need to test max y -> min y
      marsRover.moveForward
      assert(marsRover.getCurrentPosition == Point(0,0))

    }

    "move from (x, min y) to (x, max y) - when on the edge and vector pointing down" in {

      val marsRover = MarsRover(Point(0,0), Array(Array(0,0,0), Array(0,0,0), Array(0,0,0)))

      //Let MarsRove vector point downwards - need to test min y -> max y case
      marsRover.rotateClockwise
      marsRover.rotateClockwise
      marsRover.moveForward

      assert(marsRover.getCurrentPosition == Point(0,2))

    }

    "move from (max x, y) to (min x, y) - when on the edge and vector pointing right" in {

      val marsRover = MarsRover(Point(2,0), Array(Array(0,0,0), Array(0,0,0), Array(0,0,0)))

      //Let MarsRove vector point to the right - need to test max x -> min x case
      marsRover.rotateClockwise
      marsRover.moveForward

      assert(marsRover.getCurrentPosition == Point(0,0))

    }

    "move from (min x, y) to (max x, y) - when on the edge and vector pointing left" in {

      val marsRover = MarsRover(Point(0,0), Array(Array(0,0,0), Array(0,0,0), Array(0,0,0)))

      //Let MarsRove vector point to the left - need to test min x -> max x case
      marsRover.rotateAntiClowise
      marsRover.moveForward

      assert(marsRover.getCurrentPosition == Point(2,0))

    }

  }

}
