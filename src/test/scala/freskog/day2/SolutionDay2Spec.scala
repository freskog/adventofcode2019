package freskog.day2

import freskog.day2.SolutionDay2.{Position, interpreter, memory, asList}
import freskog.day2.TestUtilsDay2._
import zio.ZIO
import zio.test._


object SolutionDay2Spec extends DefaultRunnableSpec(
  suite("All given tests for day2")(
    testM("test1")(hasExpectedValueAtPosZero(List(1,0,0,0,99), List(2,0,0,0,99))),
    testM("test2")(hasExpectedValueAtPosZero(List(2,3,0,3,99), List(2,3,0,6,99))),
    testM("test3")(hasExpectedValueAtPosZero(List(2,4,4,5,99,0), List(2,4,4,5,99,9801))),
    testM("test4")(hasExpectedValueAtPosZero(List(1,1,1,4,99,5,6,0,99), List(30,1,1,4,2,5,6,0,99))),
  )
)

object TestUtilsDay2 {
  def hasExpectedValueAtPosZero(input:List[Int], expected:List[Int]): ZIO[Any, Nothing, TestResult] = {
    val result = (interpreter(Position(0)) *> asList).provideM(memory(input.toArray))
    assertM(result, Assertion.hasSameElements(expected))
  }

}