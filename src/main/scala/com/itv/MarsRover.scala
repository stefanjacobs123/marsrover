package com.itv

/**
  * Point in 2D space.
  *
  * @param x x-coordinate
  * @param y y-coordinate
  *
  */
case class Point(x: Int, y: Int) {
  require(x >= 0)
  require(y >= 0)
}

/**
  * Unit vector describing the clockwise (-90 degrees) and anti-clockwise (90 degrees) rotation. The direction of vector
  * rotation is counterclockwise.
  *
  * Given point (x, y), rotating Q degrees, the new result will be (x', y'):
  *
  * x' = x.cos(Q) - y.sin(Q)
  * y' = x.sin(Q) - y.cos(Q)
  *
  * Given Q=90 (anti-clockwise) or Q=-90 (clockwise), cos(Q) will always be 0. Therefore:
  *
  * x' = -y.sin(Q)
  * y' =  x.sin(Q)
  *
  * Anti-clockwise (90) => x' =  y
  *                        y' = -x
  * Clockwise (-90)     => x' = -y
  *                        y' =  x
  *
  * The default direction is upwards (0, 1) - we define this ourselves
  *
  * @param x x-direction
  * @param y y-direction
  */
case class UnitVector(x: Int = 0, y: Int = 1) {
  require(x <=1 && x >= -1)
  require(y <=1 && y >= -1)
  def rotateClockwise: UnitVector = UnitVector(y, -x)
  def rotateAntiClockwise: UnitVector = UnitVector(-y, x)
}


/**
  * MarsRover's current location in a 2D space.
  *
  * The MarsRover can freely rotate and move in the 2D [[grid]] and will appear on the other side if moving across edges.
  *
  * @param startingPosition starting position of the MarsRover in the [[grid]]
  * @param grid grid in which the rover can move - all x and y values will always be positive.
  */
case class MarsRover(startingPosition: Point, grid: Array[Array[Int]]) {
  private var currentPosition = startingPosition
  private var unitVector = UnitVector()

  def getCurrentPosition: Point = currentPosition
  def getUnitVector: UnitVector = unitVector

  /**
    * Move in direction of the vector.
    */
  def moveForward(): Unit = {

    unitVector match {
      case UnitVector(x, 0) if x + currentPosition.x == -1 =>
        // Move horizontally across left-most boundary, to max boundary
        // y-coordinate stays unchanged
        currentPosition = Point(
          x = grid.head.length - 1,
          y = currentPosition.y
        )
      case UnitVector(x, 0) if x + currentPosition.x == grid.head.length =>
        // Move horizontally across right-most boundary, to min boundary
        // y-coordinate stays unchanged
        currentPosition = Point(
          x = 0,
          y = currentPosition.y
        )
      case UnitVector(0, y) if y + currentPosition.y == -1 =>
        // Move horizontally across left-most boundary, to max boundary
        // x-coordinate stays unchanged
        currentPosition = Point(
          x = currentPosition.x,
          y = grid.length - 1
        )
      case UnitVector(0, y) if y + currentPosition.y == grid.length =>
        // Move horizontally across left-most boundary, to max boundary
        // x-coordinate stays unchanged
        currentPosition = Point(
          x = currentPosition.x,
          y = 0
        )
      case UnitVector(x, y) =>
        // Not on the edges - move in direction of vector
        currentPosition = Point(
          x = currentPosition.x + x,
          y = currentPosition.y + y
        )

    }

  }

  //rotate the vector of the MarsRover clockwise
  def rotateClockwise(): Unit  = {
    unitVector = unitVector.rotateClockwise
  }

  //rotate the vector of the MarsRover anti-clockwise
  def rotateAntiClowise(): Unit = {
    unitVector = unitVector.rotateAntiClockwise
  }

}
