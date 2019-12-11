package freskog.day10

import freskog.day10.SolutionDay10.{Line, Point}
import zio.test._

object SolutionDay10Spec extends DefaultRunnableSpec ({


  val example = """.#....#####...#..
                  |##...##.#####..##
                  |##...#...#.#####.
                  |..#.....#...###..
                  |..#.#.....#....##""".stripMargin

  val allPoints = Point.from(example)

  suite("Day 10")(
    suite("PartTwo")(
    )
  )
})
