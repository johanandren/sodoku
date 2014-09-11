import Util.timeS

object Main extends App {

  val hardestSodokuEver = Board(
    (0, 0) -> 8,

    (1, 2) -> 7,
    (1, 3) -> 5,
    (1, 8) -> 9,

    (2, 1) -> 3,
    (2, 6) -> 1,
    (2, 7) -> 8,

    (3, 1) -> 6,
    (3, 5) -> 1,
    (3, 7) -> 5,

    (4, 2) -> 9,
    (4, 4) -> 4,

    (5, 3) -> 7,
    (5, 4) -> 5,

    (6, 2) -> 2,
    (6, 4) -> 7,
    (6, 8) -> 4,

    (7, 5) -> 3,
    (7, 6) -> 6,
    (7, 7) -> 1,

    (8, 6) -> 8
  )

  println(hardestSodokuEver)
  val (result, s) = timeS(Solver.parallelSolve(hardestSodokuEver))

  result.fold(
    println(s"Couldn't solve it in $s s :(")
  )(solved => println(s"Solved it in $s s:\n\n" + solved))



}