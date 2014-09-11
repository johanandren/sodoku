import scala.collection.immutable.NumericRange

object Util {

  def range(start: Byte, end: Byte): NumericRange[Byte] = new NumericRange.Inclusive[Byte](start, end, 1)

  def timeS[A](block: => A): (A, Float) = {
    val before = System.nanoTime()
    val result = block
    val after = System.nanoTime()
    val s = (after - before) / 1000000000F
    (result, s)
  }

}
