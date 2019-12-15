package freskog.day11

import freskog.day9.Memory
import freskog.decodeCommaSeparatedAsMap
import zio.{ Ref, ZIO }
import zio.test._

object SolutionDay11Spec
    extends DefaultRunnableSpec({

      val env: ZIO[Any, Nothing, RobotIO with Memory] =
        for {
          map  <- Ref.make(Map.empty[Pos, List[Paint]])
          pos  <- Ref.make(Pos(0, 0))
          dir  <- Ref.make[Direction](U)
          mode <- Ref.make[Mode](WaitingForPaint)
          mem  <- decodeCommaSeparatedAsMap("freskog/day11/input-day11.txt").orDie >>= Ref.make
        } yield
          new RobotIO with Memory.Live {
            override val visited: Ref[Map[Pos, List[Paint]]] = map
            override val currentPos: Ref[Pos]                = pos
            override val currentMode: Ref[Mode]              = mode
            override val storage: Ref[Map[BigInt, BigInt]]   = mem
            override val facing: Ref[Direction]              = dir
          }

      val getCurrentPaint =
        ZIO.accessM[RobotIO](_.getCurrentPaint)

      val paintItWhite =
        ZIO.accessM[RobotIO](_.paintCurrentPos(1))

      suite("SolutionDay11")(
        suite("Part one")(
          testM("read paint returns Black")(assertM(getCurrentPaint.provideM(env), Assertion.equalTo(Black))),
          testM("read paint after write")(assertM((paintItWhite *> getCurrentPaint).provideM(env), Assertion.equalTo(White)))
        )
      )

    })
