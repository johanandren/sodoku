
object Solver {

  def solve(start: Board): Option[Board] = {
    val emptyLeft = start.emptyCoordinates
    if (emptyLeft.isEmpty) {
      if (start.valid) Some(start)
      else {
        None
      }
    } else {
      // try all values for this cell and recurse
      val coords = emptyLeft.head
      start.validValuesForCell(coords)
        .toStream
        .map(value => solve(start.put(coords, value))).collectFirst {
          case Some(solution) => solution
        }

    }
  }

  def parallelSolve(start: Board): Option[Board] = {
    val emptyLeft = start.emptyCoordinates
    if (emptyLeft.isEmpty) {
      if (start.valid) Some(start)
      else None
    } else {
      // try all values for this cell and recurse
      val coords = emptyLeft.head
      start.validValuesForCell(coords)
        .par
        .map(value => solve(start.put(coords, value)))
        .find(_.isDefined)
        .map(_.get)

    }
  }

}