package markatta

import Util._

/** (col, row) */
case class Coord(x: Byte, y: Byte) {

  val blockNo = x / 3 + (y / 3 * 3)

}

object Board {

  def apply(prefilled: ((Int, Int), Int)*): Board =
    new Board(prefilled.map { case ((x, y), value) => Coord(x.toByte, y.toByte) -> value.toByte }.toMap)

  private[Board] val across = range(0, 8)

  // array but for optimization reasons, so think of it as immutable
  private[Board] val allCoords =
    for {
      x <- across
      y <- across
    } yield Coord(x, y)
  val all = range(1, 9).toSet

  // array but for optimization reasons, so think of it as immutable
  private[Board] val allBlocks: Array[Coord] =
    (for {
      x <- range(0, 2)
      y <- range(0, 2)
    } yield Coord(x, y)).toArray
}

/**
 * @param slots Vector(x/col)(y/row)
 */
class Board(slots: Map[Coord, Byte] = Map()) {

  import Board._

  def put(coords: Coord, value: Byte): Board = new Board(slots + (coords ->  value))

  def empty(coord: Coord): Boolean = !slots.contains(coord)

  // TODO sort these so that the ones with only one empty slot left will get prioritized
  def emptyCoordinates: Seq[Coord] =
    allCoords.filterNot(slots.contains)

  def validValuesForCell(coords: Coord): Set[Byte] = {
    if (slots.contains(coords)) Set.empty[Byte]
    else all -- usedNumbers(coords)
  }

  def usedNumbers(coords: Coord): Set[Byte] =
    slots.collect {
      // dianas clever solution to only pass the values once
      case (c @ Coord(x, y), value) if coords.x == x || coords.y == y || coords.blockNo == c.blockNo => value
    }.toSet


  def valid = {
    def columnsAndRowsUnique =
      across.forall(pos => sequenceValid(row(pos)) && sequenceValid(column(pos)))

    def blocksUnique =
      allBlocks.forall(coords => blockValid(block(coords.blockNo)))

    columnsAndRowsUnique && blocksUnique
  }

  /**
   * @return A 0-2x0-2 subblock of the board
   */
  def block(blockNo: Int): Map[Coord, Byte] =
    slots.filter(t => t._1.blockNo == blockNo)

  def blockValid(block: Map[Coord, Byte]): Boolean = sequenceValid(block.values)

  def sequenceValid(sequence: Iterable[Byte]): Boolean = sequence.size == sequence.toSet.size

  def row(y: Byte): Seq[Byte] =
    slots.collect { case (c, value) if c.y == y => value }.toSeq

  def column(x: Byte): Seq[Byte] =
    slots.collect { case (c, value) if c.x == x => value }.toSeq

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