package freskog.day3

import zio.test._

import SolutionDay3._

object SolutionDay3Spec extends DefaultRunnableSpec ({

  def calcDist(input:List[List[String]]) =
    calculateClosestIntersection(input).map(distanceToOrigin).orDieWith(new IllegalArgumentException(_))

  def steps(input:List[List[String]]) =
    calculateFewestSteps(input)

  val inputDirections = List(List("R2","U2","L1","D2"))
  val expectedLines = List(Line(Point(0,0),R,2), Line(Point(2,0),U,2), Line(Point(2,2),L,1), Line(Point(1,2),D,2))
  val expectedPoints =
    List(
      List(Point(0,0),Point(1,0),Point(2,0),Point(2,1),Point(2,2),Point(1,2),Point(1,1),Point(1,0))
    )


  val input1 =
    List(
      List("R8","U5","L5","D3"),
      List("U7","R6","D4","L4")
    )

  val expected1 = 6

  val input2 =
    List(
      List("R75","D30","R83","U83","L12","D49","R71","U7","L72"),
      List("U62","R66","U55","R34","D71","R55","D58","R83")
    )
  val expected2 = 159

  val input3 =
    List(
      List("R98","U47","R26","D63","R33","U87","L62","D20","R33","U53","R51"),
      List("U98","R91","D20","R16","D67","R40","U7","R15","U6","R7")
    )
  val expected3 = 135

  suite("Example input for day3") (
    suite("PartOne")(
      test("parseLine")(assert(parseLine(inputDirections.head),Assertion.isRight(Assertion.hasSameElements(expectedLines)))),
      testM("generateAllPoints")(assertM(generateAllPointsFrom(inputDirections),Assertion.hasSameElements(expectedPoints))),
      testM("first sequence")(assertM(calcDist(input1),Assertion.equalTo(expected1))),
      testM("second sequence")(assertM(calcDist(input2),Assertion.equalTo(expected2))),
      testM("third sequence")(assertM(calcDist(input3),Assertion.equalTo(expected3)))
    ),
    suite("PartTwo")(
      testM("first sequence")(assertM(steps(input1),Assertion.equalTo((Point(6,5),30))))
    )
  )

})


