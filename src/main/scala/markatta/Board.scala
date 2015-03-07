package markatta

/** (col, row) */
case class Coord(x: Byte, y: Byte)

object Board {

  def apply(prefilled: ((Int, Int), Int)*): Board =
    new Board(prefilled.map { case ((x, y), value) => Coord(x.toByte, y.toByte) -> value.toByte }.toMap)
}

/**
 * @param slots Vector(x/col)(y/row)
 */
class Board(slots: Map[Coord, Byte] = Map()) {

  import markatta.Util.range

  private val across = range(0, 8).toArray

  def put(coords: Coord, value: Byte): Board = new Board(slots + (coords ->  value))

  def empty(coord: Coord): Boolean = !hasValue(coord)

  def hasValue(coord: Coord): Boolean = slots.contains(coord)

  private val allCoords =
    for {
      x <- across
      y <- across
    } yield Coord(x, y)

  def emptyCoordinates: Seq[Coord] =
    allCoords.foldLeft( Set[Coord]()) { (set, coord) =>
      if (empty(coord)) set + coord
      else set
    }.toSeq.sortBy(c => c.x * 10 + c.y)

  private val all = range(1, 9).toSet
  def validValuesForCell(coords: Coord): Set[Byte] = {
    if (slots.contains(coords)) Set()
    else {
      all --
        column(coords.x).toSet --
        row(coords.y).toSet --
        block(Coord(blockCoords(coords.x), blockCoords(coords.y))).values.toSet
    }
  }

  def valid = {
    def columnsAndRowsUnique =
      across.forall(pos => sequenceValid(row(pos)) && sequenceValid(column(pos)))

    def blocksUnique =
      allBlocks.forall(coords => blockValid(block(coords)))

    columnsAndRowsUnique && blocksUnique
  }


  private val allBlocks =
    (for {
      x <- range(0, 2)
      y <- range(0, 2)
    } yield Coord(x, y)).toArray

  /**
   * @param coords block column and row 0 - 2
   * @return A 0-2x0-2 subblock of the board
   */
  def block(coords: Coord): Map[Coord, Byte] = {
    allBlocks.foldLeft(Map[Coord, Byte]()) { (map: Map[Coord, Byte], cellCoord: Coord) =>
      val Coord(cellX, cellY) = cellCoord
      val Coord(x, y) = coords
      val actualCoord = Coord((x * 3.toByte + cellX).toByte, (y * 3.toByte + cellY).toByte)
      slots.get(actualCoord)
        .fold(map)(value => map + (cellCoord -> value))
    }
  }

  def blockCoords(coord: Byte): Byte = (coord / 3).toByte


  def blockValid(block: Map[Coord, Byte]): Boolean = sequenceValid(block.values)

  def sequenceValid(sequence: Iterable[Byte]): Boolean = sequence.size == sequence.toSet.size

  def row(y: Byte): IndexedSeq[Byte] =
    across.foldLeft(IndexedSeq[Byte]()) { (acc, x) =>
      val coord = Coord(x, y)
      slots.get(coord).fold(acc)(value => acc :+ value)
    }

  def column(x: Byte): IndexedSeq[Byte] =
    across.foldLeft(IndexedSeq[Byte]()) { (acc, y) =>
      val coord = Coord(x, y)
      slots.get(coord).fold(acc)(value => acc :+ value)
    }

  override def toString = {
    val builder = StringBuilder.newBuilder
    val horizLine = "x---x---x---x\n"
    def drawRow(y: Byte): Unit = {
      def drawCell(x: Byte): Unit = {
        builder ++= slots.get(Coord(x, y)).map(_.toString).getOrElse(" ")
      }
      builder += '|'
      range(0, 2).foreach(drawCell)
      builder += '|'
      range(3, 5).foreach(drawCell)
      builder += '|'
      range(6, 8).foreach(drawCell)
      builder ++= "|\n"

    }

    range(0, 8).foreach { y =>
      if (y % 3 == 0) builder ++= horizLine
      drawRow(y)
    }

    builder ++= horizLine
    builder.toString()
  }

}