package freskog.day9

import zio.{ Queue, ZIO }
import zio.test._

object SolutionDay9Spec
    extends DefaultRunnableSpec({

      val copyOfMe         = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
      val mulLargeN        = "1102,34915192,34915192,7,4,7,99,0"
      val printLargeN      = "104,1125899906842624,99"
      val verifySetRelVal  = "109,1,1101,1000,0,100,109,104,204,-5,99"
      val verifySetRelAddr = "109,1,1101,1000,0,100,1101,104,0,50,9,50,204,-5,99"
      val verifySetRelRel  = "109,1,1101,1000,0,100,1101,104,0,50,209,49,204,-5,99"

      def convertToMap(s: String): Map[BigInt, BigInt] =
        s.split(",").toList.zipWithIndex.foldLeft(Map.empty[BigInt, BigInt]) {
          case (acc, (cell, idx)) => acc.updated(BigInt(idx), BigInt(cell))
        }
      def exec(program: String): ZIO[Any, Nothing, String] =
        (Queue.unbounded[BigInt] zip Queue.unbounded[BigInt]).flatMap {
          case (in, out) => SolutionDay9.runProgram(convertToMap(program), in, out).map(_.reverse).map(_.mkString(","))
        }

      suite("Solutions day 9")(
        suite("part one")(
          testM("verifySetRelVal")(assertM(exec(verifySetRelVal), Assertion.equalTo("1000"))),
          testM("verifySetRelAddr")(assertM(exec(verifySetRelAddr), Assertion.equalTo("1000"))),
          testM("verifySetRelRel")(assertM(exec(verifySetRelRel), Assertion.equalTo("1000"))),
          testM("copy ok")(assertM(exec(copyOfMe), Assertion.equalTo(copyOfMe))),
          testM("write big int")(assertM(exec(mulLargeN), Assertion.equalTo("1219070632396864"))),
          testM("print very large in")(assertM(exec(printLargeN), Assertion.equalTo("1125899906842624")))
        )
      )
    })
