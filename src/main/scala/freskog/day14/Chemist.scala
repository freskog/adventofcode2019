package freskog.day14

import cats.Functor
import higherkindness.droste.{ scheme, Algebra, Coalgebra }
import zio.stream.ZStream
import zio.{ Ref, UIO, ZIO }

trait Chemist {
  val chemist: Chemist.Service[Any]
}

object Chemist {
  trait Service[R] {
    def streamFor(reactionRules: String, qty: Int): ZIO[Any, Nothing, Map[String,Int]]
  }

  trait Live extends Chemist {

    val parser: Parser.Service[Any]

    val pool: Ref[Map[String, Int]]
    val oreCount: Ref[Long]

    val rawOre: ZStream[Any, Nothing, OneUnitOf] =
      ZStream.succeed(OneUnitOf("ORE")).tap(_ => oreCount.update(_ + 1L)).forever

    def updatePool(name: String, qty: Int): UIO[Map[String, Int]] =
      pool.update(m => m.updated(name, qty))

    def batched(name: String, qty: Int, dep: ZStream[Any, Nothing, OneUnitOf]): ZStream[Any, Nothing, OneUnitOf] =
      ZStream.repeatEffect {
        pool.get
          .map(_.get(name))
          .flatMap {
            case None | Some(0) => dep.take(1).runDrain *> updatePool(name, qty - 1) *> ZIO.succeed(OneUnitOf(name))
            case Some(n)        => updatePool(name, n - 1) *> ZIO.succeed(OneUnitOf(name))
          }
      }

    def joinInputs(name: String,
                   input: List[(Int, ZStream[Any, Nothing, OneUnitOf])]): ZStream[Any, Nothing, OneUnitOf] =
      ZStream.repeatEffect(ZIO.foreach_(input) { case (n, dep) => dep.take(n).runDrain }).as(OneUnitOf(name))

    def buildReactions(reactions: Map[String, Reaction]): Coalgebra[ReactionF, Chemical] =
      Coalgebra {
        case Chemical("ORE", qty) => OreF(qty)
        case Chemical(name, qty)  => ChemicalF(name, reactions(name).provides.quant, reactions(name).input.map(c => (c.quant, c)))
      }

    def buildStream: Algebra[ReactionF, ZStream[Any, Nothing, OneUnitOf]] =
      Algebra {
        case OreF(_)                  => rawOre
        case ChemicalF(n, qty, needs) => batched(n, qty, joinInputs(n, needs))
      }

    def reactionTree: Algebra[ReactionF, String] =
      Algebra {
        case OreF(qty) => s"OreF($qty)"
        case ChemicalF(n, qty, needs) => s"""ChemicalF($n, $qty, ${needs.mkString("[",",","]")}"""
      }

    override val chemist: Service[Any] = (reactionRules: String, qty: Int) =>
      ZStream
        .unwrap(
          parser
            .parseReactions(reactionRules)
            .map(
              m => scheme.hylo(buildStream, buildReactions(m))(Functor[ReactionF])(Chemical("FUEL", 1))
            )
        )
        .take(qty)
        .runCollect *> pool.get
  }
}

sealed abstract class ReactionF[A]
case class OreF[A](qty: Int)                                                extends ReactionF[A]
case class ChemicalF[A](name: String, provides: Int, needs: List[(Int, A)]) extends ReactionF[A]
object ReactionF {
  implicit val func: Functor[ReactionF] = new Functor[ReactionF] {
    override def map[A, B](fa: ReactionF[A])(f: A => B): ReactionF[B] = fa match {
      case OreF(qty)              => OreF(qty)
      case ChemicalF(n, q, needs) => ChemicalF(n, q, needs.map { case (n, a) => (n, f(a)) })
    }
  }
}
