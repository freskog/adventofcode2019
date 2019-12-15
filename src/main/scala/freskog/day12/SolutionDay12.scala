package freskog.day12

import java.io.IOException

import zio._
import zio.console.Console

import scala.annotation.tailrec
import scala.util.matching.Regex

object SolutionDay12 extends App {

  case class Pos(x: Int, y: Int, z: Int) {

    def render:String =
      String.format("pos=<x=%3d, y=  %3d, z= %3d>",x,y,z)
  }
  object Pos {
    val posPattern: Regex = """<x=(-?\d+), y=(-?\d+), z=(-?\d+)>""".r

    def from(line: String): Pos = line match {
      case posPattern(x, y, z) => Pos(x.toInt, y.toInt, z.toInt)
    }
  }

  case class Vel(x: Int, y: Int, z: Int) {

    def calcChange(p1:Int, p2:Int):Int =
      if(p1 < p2) 1 else if(p1 > p2) -1 else 0
    def from(p1:Pos,p2:Pos):Vel =
      Vel(x + calcChange(p1.x,p2.x),y + calcChange(p1.y,p2.y),z + calcChange(p1.z,p2.z))
    def render:String =
      String.format("vel=<x= %3d, y= %3d, z= %3d>",x,y,z)
  }

  case class Moon(id: Int, pos: Pos, vel: Vel) {
    def applyGravity(positions:List[Pos]):Moon =
      copy(vel = positions.foldLeft(vel)(_.from(pos, _)))
    def applyVelocity:Moon =
      copy(pos = Pos(pos.x + vel.x, pos.y + vel.y, pos.z + vel.z))
    def potential:Int =
      Math.abs(pos.x) + Math.abs(pos.y) + Math.abs(pos.z)
    def kinetic:Int =
      Math.abs(vel.x) + Math.abs(vel.y) + Math.abs(vel.z)
    def total:Int =
      potential * kinetic
    def render:String = s"$id: ${pos.render}, ${vel.render}"
  }

  case class World(pairings: Map[Ref[Moon], List[Ref[Moon]]])
  object World {
    def from(moons: List[Moon]): ZIO[Any, Nothing, World] =
      ZIO
        .foreach(moons)(Ref.make)
        .map(moonRefs => World(moonRefs.map(mRef => mRef -> moonRefs.filterNot(_ == mRef)).toMap))
  }


  def updateVelocities(world:World): ZIO[Any, Nothing, List[Moon]] =
    ZIO.foreach(world.pairings) {
      case (m, moonRefs) => ZIO.foreach(moonRefs)(_.get.map(_.pos)).flatMap(poses => m.update(_.applyGravity(poses)))
    }

  def updatePositions(world:World): ZIO[Any, Nothing, List[Moon]] =
    ZIO.foreach(world.pairings.keys)(_.update(_.applyVelocity))

  def nextStep(world:World): ZIO[Any, Nothing, World] =
    updateVelocities(world) *> updatePositions(world) *> ZIO.succeed(world)

  def totalEnergy(world:World): ZIO[Any, Nothing, Int] =
    ZIO.foreach(world.pairings.keys)(_.get.map(_.total)).map(_.sum)

  def renderWorld(world:World): ZIO[Any, Nothing, String] =
    ZIO.foreach(world.pairings.keys)(_.get.map(_.render)).map(_.mkString("\n"))

  def readWorld(path:String): ZIO[Any, IOException, World] =
    freskog
      .decodeLines(path)
      .map(Pos.from)
      .mapAccum(0)((n, p) => (n + 1, Moon(n, p, Vel(0, 0, 0))))
      .runCollect
      .flatMap(World.from)

  val partOne: ZIO[Console, IOException, Unit] =
    for {
      world <- readWorld("freskog/day12/input-day12.txt")
          _ <- nextStep(world).repeat(Schedule.recurs(999))
          e <- totalEnergy(world)
          _ <- console.putStrLn(s"total energy after 1000 steps $e")
    } yield ()

  type Hash = List[(Int,Int)]

  def calculcateCycleLength(hashes:Ref[Set[Hash]], world:World, n:Long)(posf:Moon => (Int,Int)):ZIO[Console, Nothing, Long] =
    for {
      ps <- ZIO.foreach(world.pairings.keys)(_.get.map(posf))
      hasP <- hashes.get.map(_.contains(ps))
      w <- nextStep(world)
      r <- if(hasP) ZIO.succeed(n) else hashes.update(_ + ps) *> calculcateCycleLength(hashes, w, n + 1)(posf)
    } yield r

  @tailrec
  def calculateCyclesJoinPoint(x:Long,y:Long,z:Long,a:Long,b:Long,c:Long):Long =
    if(a*x == b*y && b*y == c*z) a*x
    else if(a*x == math.min(math.min(a*x,b*y),c*z)) calculateCyclesJoinPoint(x,y,z,a+1,b,c)
    else if(b*y == math.min(math.min(a*x,b*y),c*z)) calculateCyclesJoinPoint(x,y,z,a,b+1,c)
    else calculateCyclesJoinPoint(x,y,z,a,b,c+1)

  def allCycleLengths(world: World): ZIO[Console, Nothing, Unit] =
    for {
      hash <- Ref.make(Set[Hash]())
      xCyc <- hash.update(_ => Set[Hash]()) *> calculcateCycleLength(hash, world, 0)(m => (m.pos.x, m.vel.x))
      yCyc <- hash.update(_ => Set[Hash]()) *> calculcateCycleLength(hash, world, 0)(m => (m.pos.y, m.vel.y))
      zCyc <- hash.update(_ => Set[Hash]()) *> calculcateCycleLength(hash, world, 0)(m => (m.pos.z, m.vel.z))
        _  <- console.putStrLn(s"cycles lengths are x = $xCyc, y = $yCyc and z = $zCyc")
        _  <- console.putStrLn(s"cycles join after ${calculateCyclesJoinPoint(xCyc,yCyc,zCyc,1,1,1)} steps")
    } yield ()

  val partTwo: ZIO[Console, IOException, Unit] =
    for {
      world <- readWorld("freskog/day12/input-day12.txt")
      _ <- allCycleLengths(world)
    } yield ()

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (partOne *> partTwo).orDie.as(0)
}
