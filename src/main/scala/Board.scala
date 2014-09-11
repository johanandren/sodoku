import scala.collection.immutable.NumericRange

object Board {

  def apply(prefilled: ((Int, Int), Int)*): Board =
    new Board(prefilled.map { case ((x, y), value) => (x.toByte, y.toByte) -> value.toByte }.toMap)
}

/**
 * @param slots Vector(x/col)(y/row)
 */
class Board(slots: Map[(Byte, Byte), Byte] = Map()) {

  import Util.range

  private val across = range(0, 8)

  def put(coords: (Byte, Byte), value: Byte): Board =
    new Board(slots + (coords ->  value))

  def get(x: Byte, y: Byte): Option[Byte] =
    slots.get((x, y))

  def empty(x: Byte, y: Byte): Boolean =
    !slots.contains((x, y))

  def emptyCoordinates: Seq[(Byte, Byte)] =
    (for {
      x <- across
      y <- across
    } yield
      if (empty(x, y)) Some((x, y))
      else None
      ).flatten.toSet.toSeq.sorted

  private val all = range(1, 9).toSet
  def validValuesForCell(coords: (Byte, Byte)): Set[Byte] = {
    if (slots.contains(coords)) Set()
    else {
      val (x, y) = coords
      // todo remove values in block
      all -- column(x).toSet -- row(y).toSet -- block(blockCoords(x), blockCoords(y)).values.toSet
    }
  }


  def hasValue(x: Byte, y: Byte): Boolean =
    slots.contains((x, y))

  def valid = {
    def columnsAndRowsUnique =
      !(for {
        pos <- across
      } yield {
        sequenceValid(row(pos.toByte)) && sequenceValid(column(pos.toByte))
      }).contains(false)

    def blocksUnique =
      !(for {
        x <- range(0, 2)
        y <- range(0, 2)
      } yield blockValid(block(x, y))).contains(false)

    columnsAndRowsUnique && blocksUnique
  }

  /**
   * @param x block column 0 - 2
   * @param y block row 0 - 2
   * @return A 0-2x0-2 subblock of the board
   */
  def block(x: Byte, y: Byte): Map[(Byte, Byte), Byte] = {
    range(0, 2).map { cellX =>
      range(0, 2).map { cellY =>
        slots.get(((x * 3 + cellX).toByte, (y * 3 + cellY).toByte))
          .map(value => (cellX, cellY) -> value)
      }
    }.flatten.flatten.toMap
  }

  def blockCoords(coord: Byte): Byte = (coord / 3).toByte


  def blockValid(block: Map[(Byte, Byte), Byte]): Boolean = sequenceValid(block.values)

  def sequenceValid(sequence: Iterable[Byte]): Boolean = sequence.size == sequence.toSet.size

  def row(y: Byte): IndexedSeq[Byte] =
    (for {
      x <- across
    } yield slots.get((x, y))).flatten

  def column(x: Byte): IndexedSeq[Byte] =
    (for {
      y <- across
    } yield slots.get(x, y)).flatten





  override def toString = {
    val builder = StringBuilder.newBuilder
    val horizLine = "x---x---x---x\n"
    def drawRow(y: Byte): Unit = {
      def drawCell(x: Byte): Unit = {
        builder ++= slots.get((x, y)).map(_.toString).getOrElse(" ")
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

class BigCell(slots: IndexedSeq[IndexedSeq[Option[Byte]]])
