package freskog.day5

import zio.ZIO
import zio.test.Assertion.hasSameElements
import zio.test._

object SolutionDay5Spec
    extends DefaultRunnableSpec({

      val end       = Array(99)
      val echo      = Array(3, 1, 4, 1, 99)
      val addLit    = Array(1101, 1, 2, 3, 4, 3, 99)
      val addLitNeg = Array(1101, -3, -2, 3, 4, 3, 99)

      val printZeroOrOne    = Array(3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9)
      val printZeroOrOneLit = Array(3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1)

      val isEqualTo8     = Array(3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8)
      val isLessThan8    = Array(3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8)
      val isEqualTo8Lit  = Array(3, 3, 1108, -1, 8, 3, 4, 3, 99)
      val isLessThan8Lit = Array(3, 3, 1107, -1, 8, 3, 4, 3, 99)

      val zeroIfzero    = Array(3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9)
      val zeroIfzeroLit = Array(3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1)

      val onekPlusMinus1 = Array(3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36, 98, 0,
        0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99)

      def run(program: Array[Int], input: List[Int]): ZIO[Any, Nothing, List[Int]] =
        ZIO.accessM[Computer](_.computer.interpreter(Position(0))).provideM(SolutionDay5.env(program, input))

      suite("Day 5")(
        suite("PartOne")(
          testM("end")(assertM(run(end, Nil), hasSameElements(Nil))),
          testM("echo")(assertM(run(echo, List(1)), hasSameElements(List(1)))),
          testM("addLit")(assertM(run(addLit, Nil), hasSameElements(List(3)))),
          testM("addLitNeg")(assertM(run(addLitNeg, Nil), hasSameElements(List(-5))))
        ),
        suite("PartTwo")(
          testM("printZero")(assertM(run(printZeroOrOne, List(1)), hasSameElements(List(1)))),
          testM("printZeroLit")(assertM(run(printZeroOrOneLit, List(0)), hasSameElements(List(0)))),
          testM("printOne")(assertM(run(printZeroOrOne, List(1)), hasSameElements(List(1)))),
          testM("printOneLit")(assertM(run(printZeroOrOneLit, List(1)), hasSameElements(List(1)))),
          testM("8 == 8")(assertM(run(isEqualTo8, List(8)), hasSameElements(List(1)))),
          testM("8 != 7")(assertM(run(isEqualTo8, List(7)), hasSameElements(List(0)))),
          testM("8 == 8 (Lit)")(assertM(run(isEqualTo8Lit, List(8)), hasSameElements(List(1)))),
          testM("8 != 7 (Lit)")(assertM(run(isEqualTo8Lit, List(7)), hasSameElements(List(0)))),
          testM("7 < 8")(assertM(run(isLessThan8, List(7)), hasSameElements(List(1)))),
          testM("8 < 8 is false")(assertM(run(isLessThan8, List(8)), hasSameElements(List(0)))),
          testM("7 < 8 (Lit)")(assertM(run(isLessThan8Lit, List(7)), hasSameElements(List(1)))),
          testM("8 < 8 is false (Lit)")(assertM(run(isLessThan8Lit, List(8)), hasSameElements(List(0)))),
          testM("0 -> 0")(assertM(run(zeroIfzero, List(0)), hasSameElements(List(0)))),
          testM("1 -> 1")(assertM(run(zeroIfzero, List(1)), hasSameElements(List(1)))),
          testM("0 -> 0 (Lit)")(assertM(run(zeroIfzeroLit, List(0)), hasSameElements(List(0)))),
          testM("1 -> 1 (Lit)")(assertM(run(zeroIfzeroLit, List(1)), hasSameElements(List(1)))),
          testM("7 -> 999")(assertM(run(onekPlusMinus1, List(7)), hasSameElements(List(999)))),
          testM("8 -> 1000")(assertM(run(onekPlusMinus1, List(8)), hasSameElements(List(1000)))),
          testM("9 -> 1000")(assertM(run(onekPlusMinus1, List(9)), hasSameElements(List(1001))))
        )
      )
    })
