package freskog.day12

import freskog.day12.SolutionDay12.{World, readWorld, renderWorld}
import zio.test._

object SolutionDay12Spec extends DefaultRunnableSpec ({

  val zeroSteps = readWorld("freskog/day12/input-test-day12.txt")
  val expectedBeforeOneStep =
  """0: pos=<x= -1, y=    0, z=   2>, vel=<x=   0, y=   0, z=   0>
    |1: pos=<x=  2, y=  -10, z=  -7>, vel=<x=   0, y=   0, z=   0>
    |2: pos=<x=  4, y=   -8, z=   8>, vel=<x=   0, y=   0, z=   0>
    |3: pos=<x=  3, y=    5, z=  -1>, vel=<x=   0, y=   0, z=   0>""".stripMargin

  val afterOneStep = zeroSteps >>= SolutionDay12.nextStep
  val expectedAfterOneStep =
  """0: pos=<x=  2, y=   -1, z=   1>, vel=<x=   3, y=  -1, z=  -1>
    |1: pos=<x=  3, y=   -7, z=  -4>, vel=<x=   1, y=   3, z=   3>
    |2: pos=<x=  1, y=   -7, z=   5>, vel=<x=  -3, y=   1, z=  -3>
    |3: pos=<x=  2, y=    2, z=   0>, vel=<x=  -1, y=  -3, z=   1>""".stripMargin

  suite("SolutionDay12")(
    suite("Part one")(
      testM("world is in correct state after being read")(assertM(zeroSteps >>= renderWorld, Assertion.equalTo(expectedBeforeOneStep))),
      testM("world is in correct state after one step")(assertM(afterOneStep >>= renderWorld, Assertion.equalTo(expectedAfterOneStep)))
    )
  )
})
