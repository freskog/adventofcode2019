package freskog.day14

import cats.Functor
import higherkindness.droste.scheme
import zio.stream.ZStream
import zio.{ Ref, ZIO }
import zio.test._

object SolutionDay14Spec
    extends DefaultRunnableSpec({

      val test1 = """10 ORE => 10 A
                    |1 ORE => 1 B
                    |7 A, 1 B => 1 C
                    |7 A, 1 C => 1 D
                    |7 A, 1 D => 1 E
                    |7 A, 1 E => 1 FUEL""".stripMargin

      val test2 = """9 ORE => 2 A
                    |8 ORE => 3 B
                    |7 ORE => 5 C
                    |3 A, 4 B => 1 AB
                    |5 B, 7 C => 1 BC
                    |4 C, 1 A => 1 CA
                    |2 AB, 3 BC, 4 CA => 1 FUEL""".stripMargin

      val test3 = """157 ORE => 5 NZVS
                    |165 ORE => 6 DCFZ
                    |44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
                    |12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
                    |179 ORE => 7 PSHF
                    |177 ORE => 5 HKGWZ
                    |7 DCFZ, 7 PSHF => 2 XJWVT
                    |165 ORE => 2 GPVTF
                    |3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT""".stripMargin

      val test4 = """2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
                    |17 NVRVD, 3 JNWZP => 8 VPVL
                    |53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
                    |22 VJHF, 37 MNCFX => 5 FWMGM
                    |139 ORE => 4 NVRVD
                    |144 ORE => 7 JNWZP
                    |5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
                    |5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
                    |145 ORE => 6 MNCFX
                    |1 NVRVD => 8 CXFTF
                    |1 VJHF, 6 MNCFX => 4 RFSQX
                    |176 ORE => 6 VJHF""".stripMargin

      val test5 = """171 ORE => 8 CNZTR
                    |7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
                    |114 ORE => 4 BHXH
                    |14 VRPVC => 6 BMBT
                    |6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
                    |6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
                    |15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
                    |13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
                    |5 BMBT => 4 WPTQ
                    |189 ORE => 9 KTJDG
                    |1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
                    |12 VRPVC, 27 CNZTR => 2 XDBXC
                    |15 KTJDG, 12 BHXH => 5 XCVML
                    |3 BHXH, 2 VRPVC => 7 MZWV
                    |121 ORE => 7 VRPVC
                    |7 XCVML => 6 RJRHP
                    |5 BHXH, 4 VRPVC => 5 LTCX""".stripMargin

      val rulesTest1 =
        Map(
          "A"    -> Reaction(List(Chemical("ORE", 10)), Chemical("A", 10)),
          "B"    -> Reaction(List(Chemical("ORE", 1)), Chemical("B", 1)),
          "C"    -> Reaction(List(Chemical("A", 7), Chemical("B", 1)), Chemical("C", 1)),
          "D"    -> Reaction(List(Chemical("A", 7), Chemical("C", 1)), Chemical("D", 1)),
          "E"    -> Reaction(List(Chemical("A", 7), Chemical("D", 1)), Chemical("E", 1)),
          "FUEL" -> Reaction(List(Chemical("A", 7), Chemical("E", 1)), Chemical("FUEL", 1)),
        )


      def envWith(poolRef: Ref[Map[String, Int]]): ZIO[Any, Nothing, Chemist.Live with Parser.Live] =
        Ref.make(0L) >>= (SolutionDay14.chemist(_, poolRef))

      val env: ZIO[Any, Nothing, Chemist.Live with Parser.Live] =
        Ref.make(Map.empty[String, Int]) >>= envWith

      def parseReactions(input: String): ZIO[Any, Nothing, Map[String, Reaction]] =
        ZIO.accessM[Parser](_.parser.parseReactions(input)).provideM(env)

      val getOreCnt: ZIO[Chemist.Live, Nothing, Long] =
        ZIO.accessM[Chemist.Live](_.oreCount.get)

      def batched(name: String,
                  qty: Int,
                  dep: ZStream[Chemist.Live, Nothing, OneUnitOf]): ZStream[Chemist.Live, Nothing, OneUnitOf] =
        ZStream.accessM[Chemist.Live](chemist => chemist.batched(name, qty, dep.provide(chemist)))

      def joinInputs(
        name: String,
        input: List[(Int, ZStream[Chemist.Live, Nothing, OneUnitOf])]
      ): ZStream[Chemist.Live, Nothing, OneUnitOf] =
        ZStream.accessM[Chemist.Live](
          chemist => chemist.joinInputs(name, input.map { case (n, z) => (n, z.provide(chemist)) })
        )

      suite("SolutionDay14")(
        suite("PartOne")(
          suite("Parsing")(
            testM("Can parse test input")(assertM(parseReactions(test1), Assertion.hasSameElements(rulesTest1)))
          ),
          suite("Fetch ore from stream")(
            testM("batched uses correct amount of ore")({
              val rawOre     = ZStream.unwrap(ZIO.access[Chemist.Live](_.rawOre))
              val batchedOre = batched("A", 10, joinInputs("ORE", List((10, rawOre)))).take(1).runDrain *> getOreCnt
              assertM(batchedOre, Assertion.equalTo(10))
            }),
            testM("using and batched use correct amount of ore")({
              val rawOre                      = ZStream.unwrap(ZIO.access[Chemist.Live](_.rawOre))
              val as                          = batched("A", 10, joinInputs("A", List((10, rawOre))))
              val bs                          = batched("B", 1, joinInputs("B", List((1, rawOre))))
              val cs                          = batched("C", 1, joinInputs("C", List((7, as), (1, bs))))
              val ds                          = batched("D", 1, joinInputs("D", List((7, as), (1, cs))))
              val es                          = batched("E", 1, joinInputs("E", List((7, as), (1, ds))))
              val fuel                        = batched("FUEL", 1, joinInputs("FUEL", List((7, as), (1, es))))
              val oreCntAfterBatchedWithUsing = fuel.take(1).runDrain *> getOreCnt
              assertM(oreCntAfterBatchedWithUsing, Assertion.equalTo(31))
            }),
            testM("reaction tree is built correctly")({
              val buildStream    = ZIO.access[Chemist.Live](_.reactionTree)
              val buildReactions = ZIO.access[Chemist.Live](_.buildReactions(rulesTest1))
              val fuel           = (buildStream.zipWith(buildReactions)(scheme.hylo(_, _)(Functor[ReactionF])(Chemical("C", 1))))
              assertM(fuel,
                      Assertion.equalTo(
                        "ChemicalF(C, 1, [(7,ChemicalF(A, 10, [(10,OreF(10))]),(1,ChemicalF(B, 1, [(1,OreF(1))])]"
                      ))
            }),
            testM("test1 input uses exactly 165 ore")({
              assertM(SolutionDay14.calculateOreRequired(test1, 1), Assertion.equalTo(31))
            }),
            testM("test2 input uses exactly 165 ore")({
              assertM(SolutionDay14.calculateOreRequired(test2, 1), Assertion.equalTo(165))
            }),
            testM("test3 input uses exactly 13312 ore")({
              assertM(SolutionDay14.calculateOreRequired(test3, 1), Assertion.equalTo(13312))
            }),
            testM("test4 input uses exactly 180697 ore")({
              assertM(SolutionDay14.calculateOreRequired(test4, 1), Assertion.equalTo(180697))
            }),
            testM("test5 input uses exactly 2210736 ore")({
              assertM(SolutionDay14.calculateOreRequired(test5, 1), Assertion.equalTo(2210736))
            })
          )
        ),
        suite("Part two")(
          testM("calculate fuel usage for two fuel given input")({
            def s(n: Int) =
              Ref.make(Map.empty[String, Int]).flatMap { poolRef =>
                ZIO.accessM[Chemist](_.chemist.streamFor(test5, n)).provideM(envWith(poolRef))
              }
            val t = ZIO.foreach((1 to 3))(s)
            assertM(t, Assertion.hasSameElements(Nil))
          })
        )
      ).provideManaged(env.toManaged_)
    })
