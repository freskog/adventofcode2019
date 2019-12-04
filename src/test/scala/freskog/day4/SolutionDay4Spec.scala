package freskog.day4

import zio.test._

object SolutionDay4Spec extends DefaultRunnableSpec({

  import SolutionDay4._
  import fastparse._, NoWhitespace._



  suite("PartTwo")(
    test("112233 ok")(assert(parse("112233",onlyStrictPairs(0)(_)).isSuccess, Assertion.equalTo(true))),
    test("112230 fail")(assert(parse("112230",onlyStrictPairs(0)(_)).isSuccess, Assertion.equalTo(false))),
    test("122223 fail")(assert(parse("122223",onlyStrictPairs(0)(_)).isSuccess, Assertion.equalTo(false))),
    test("112345 ok")(assert(parse("112345",onlyStrictPairs(0)(_)).isSuccess, Assertion.equalTo(true))),
    test("102230 fail")(assert(parse("012230",onlyStrictPairs(0)(_)).isSuccess, Assertion.equalTo(false))),
    test("123444 fail")(assert(parse("123444",onlyStrictPairs(0)(_)).isSuccess, Assertion.equalTo(false))),
    test("111122 ok")(assert(parse("111122",onlyStrictPairs(0)(_)).isSuccess, Assertion.equalTo(true)))
  )

})
