package net.demo

import scala.util.Try
import scala.util.control.NonFatal

/** GADTs - Parametrically polymorphic ADT, where you are allowed to specialize a type parameter in the terms of the sum
  * type.
  *
  * Specialization the type T of Num inside the children together with being able to reconstruct this information in
  * pattern matching is known as GADTs.
  */

/*
  (Scalac knows about types in each cases)
    def zero[T](N: NumTag[T]): T =
      N match {
        case NumTag.Integer ⇒ 1 //T =:=* Int
        case NumTag.Dbl ⇒ 1.0 //T =:= Double
        case NumTag.Lng ⇒ 1L //T =:= Long
        case NumTag.Flt ⇒ 1.7f //T =:= Float }
 */

/*

Functional Scala - Next-Level Type Safety: An Intro to Generalized Algebraic Data Types https://youtu.be/1E053kld8Ac?t=331
sealed trait TypeEquality[A, B] {
  def toF[F[_]](a: F[A]): F[B]

  def fromF[F[_]](a: F[B]): F[A]
}

object TypeEquality {
  implicit def refl[A]: TypeEquality[A, A] = new TypeEquality[A, A] {
    def toF[F[_]](a: F[A]): F[A] = a

    def fromF[F[_]](a: F[A]): F[A] = a
  }
}
 */

//runMain net.demo.GADTs
object GADTs extends App {

  // Declarative encoding
  // We turn our dynamically typed model of CalculatedValue into a statically typed DSL
  // we can't use scala.math.Numeric because it's not a sum nor a product type.
  // As we use declarative encoding we want to have as many interpreters as we'd like.

  sealed trait NumTag[T]

  object NumTag {
    implicit case object Integer extends NumTag[Int]

    implicit case object Dbl extends NumTag[Double]

    implicit case object Flt extends NumTag[Float]

    implicit case object Lng extends NumTag[Long]
  }

  final case class Coercion[-A, +B](op: A => Try[B]) extends AnyVal

  // A set of coercions this DSL supports
  object Coercion {
    implicit val A = Coercion[Int, Double](int => Try(int.toDouble))

    implicit val B = Coercion[String, Double]((in: String) => Try(in.toDouble))

    implicit val C = Coercion[Double, Int]((in: Double) => Try(in.toInt))

    implicit val D = Coercion[Int, Float]((in: Int) => Try(in.toFloat))
  }

  /*
  sealed abstract case class Coercion[-In, +Out](op: In => Try[Out])
  object Coercion {
    implicit object A extends Coercion[Int, Double]((in: Int) => Try(in.toDouble))

    implicit object B extends Coercion[String, Double]((in: String) => Try(in.toDouble))

    implicit object C extends Coercion[Double, Int]((in: Double) => Try(in.toInt))

    implicit object D extends Coercion[Int, Float]((in: Int) => Try(in.toFloat))
  }
   */

  sealed trait DslElement[+A] { self =>

    def +[B >: A](that: DslElement[B])(implicit T: NumTag[B]): DslElement[B] =
      DslElement.Plus(self, that, T)

    def -[B >: A](that: DslElement[B])(implicit T: NumTag[B]): DslElement[B] =
      DslElement.Minus(self, that, T)

    def *[B >: A](that: DslElement[B])(implicit T: NumTag[B]): DslElement[B] =
      DslElement.Times(self, that, T)

    def as[B](implicit C: Coercion[A, B], T: NumTag[B]): DslElement[B] =
      DslElement.CastTo(self, C, T)

    def unary_-[B >: A](implicit T: NumTag[B]): DslElement[B] =
      DslElement.Negate(self, T)

    def f(implicit C: Coercion[A, Float], T: NumTag[Float]): DslElement[Float] =
      DslElement.CastTo(self, C, T)

    def d(implicit C: Coercion[A, Double], T: NumTag[Double]): DslElement[Double] =
      DslElement.CastTo(self, C, T)

    def l(implicit C: Coercion[A, Long], T: NumTag[Long]): DslElement[Long] =
      DslElement.CastTo(self, C, T)
  }

  object DslElement {

    final case class Plus[A](a: DslElement[A], b: DslElement[A], tag: NumTag[A]) extends DslElement[A]

    final case class Minus[A](a: DslElement[A], b: DslElement[A], tag: NumTag[A]) extends DslElement[A]

    final case class Times[A](a: DslElement[A], b: DslElement[A], tag: NumTag[A]) extends DslElement[A]

    final case class Negate[A](v: DslElement[A], tag: NumTag[A]) extends DslElement[A]

    final case class Literal[A](v: A, tag: NumTag[A]) extends DslElement[A]

    final case class CastTo[A, B](v: DslElement[A], cast: Coercion[A, B], tag: NumTag[B]) extends DslElement[B]

    def lit[A](c: A)(implicit tag: NumTag[A]): DslElement[A] = Literal(c, tag)

    implicit class DslElementOps[A](val self: A) extends AnyVal {
      def lit(implicit tag: NumTag[A]): DslElement[A] = Literal(self, tag)
    }

    implicit class CalculatedValueStrOps(val self: String) extends AnyVal {
      def fromStr[A](implicit ev: NumTag[A], sv: NumTag[A]): DslElement[A] =
        ev match {
          case NumTag.Integer => Try(self.toInt).map(Literal(_, sv)).getOrElse(throw new Exception("Error toInt !"))
          case NumTag.Dbl => Try(self.toDouble).map(Literal(_, sv)).getOrElse(throw new Exception("Error toDouble !"))
          case NumTag.Flt => Try(self.toFloat).map(Literal(_, sv)).getOrElse(throw new Exception("Error  toFloat !"))
          case NumTag.Lng => Try(self.toLong).map(Literal(_, sv)).getOrElse(throw new Exception("Error toLong !"))
        }
    }
  }

  import NumTag._
  import DslElement._

  // default interpreter
  def eval[T](v: DslElement[T]): T = {

    def plus(a: T, b: T, ev: NumTag[T]): T =
      ev match {
        case Integer => a + b
        case Dbl     => a + b
        case Flt     => a + b
        case Lng     => a + b
      }

    def minus(a: T, b: T, n: NumTag[T]): T =
      n match {
        case Integer => a - b
        case Dbl     => a - b
        case Flt     => a - b
        case Lng     => a - b
      }

    def times(a: T, b: T, n: NumTag[T]): T =
      n match {
        case Integer => a * b
        case Dbl     => a * b
        case Flt     => a * b
        case Lng     => a * b
      }

    def negate(a: T, n: NumTag[T]): T =
      n match {
        case Integer => -a
        case Dbl     => -a
        case Flt     => -a
        case Lng     => -a
      }

    v match {
      /*
        found   : net.demo.GADTs.Num[?T1] where type ?T1 <: T (this is a GADT skolem)
        [error]  required: net.demo.GADTs.Num[T]
        [error] Note: ?T1 <: T, but trait Num is invariant in type T.
        [error] You may wish to define T as +T instead. (SLS 4.5)
        [error]         plus(eval(a), eval(b), n)
      case Plus(a, b, n) => plus(eval(a), eval(b), n)
       */
      case op: Plus[T] =>
        plus(eval(op.a), eval(op.b), op.tag)
      case op: Minus[T] =>
        minus(eval(op.a), eval(op.b), op.tag)
      case op: Times[T] =>
        times(eval(op.a), eval(op.b), op.tag)
      case op: Negate[T] =>
        negate(eval(op.v), op.tag)
      case castOp: CastTo[in, out] =>
        val rawValue: in = eval(castOp.v)
        val result: out = castOp.cast
          .op(rawValue)
          .fold(
            ex =>
              throw new IllegalArgumentException(
                s"Cast error: ${rawValue.getClass.getName} -> ${castOp.tag.getClass.getName}",
                ex
              ),
            identity
          )

        result
      case Literal(v, _) =>
        v

      /*case coerce: Coerce[in, out] =>
        val rawValue = eval(coerce.v) //.asInstanceOf[coerce.c.In]
        converter(coerce.c.from, coerce.c.to)(rawValue)*/
    }
  }

  val i: Byte = 0x00
  val d: Byte = 0x01
  val f: Byte = 0x02
  val l: Byte = 0x03

  def serialize[T](v: DslElement[T]): String =
    v match {
      case Plus(a, b, _)  => s"PLUS|" + serialize(a) + "|" + serialize(b)
      case Minus(a, b, _) => s"MINUS|" + serialize(a) + "|" + serialize(b)
      case Times(a, b, _) => s"TIMES|" + serialize(a) + "|" + serialize(b)
      case Negate(v, _)   => s"NEGATE|" + serialize(v)
      case Literal(v, c) =>
        c match {
          case NumTag.Integer => s"$i:$v"
          case NumTag.Dbl     => s"$d:$v"
          case NumTag.Flt     => s"$f:$v"
          case NumTag.Lng     => s"$l:$v"
        }
      case c: CastTo[in, out] =>
        val to = c.tag match {
          case NumTag.Integer => i
          case NumTag.Dbl     => d
          case NumTag.Flt     => f
          case NumTag.Lng     => l
        }
        "CAST|" + serialize(c.v) + "->" + to

      /*case Coerce(v, c) =>
         val to = c.toSer match {
           case SerializableVal.Integer => i
           case SerializableVal.Dbl     => d
           case SerializableVal.Flt     => f
           case SerializableVal.Lng     => l
         }
         s"COERCE_TO:$to|" + serialize(v)
       */
    }

  def deserialize(line: String): DslElement[_] = ???

  // another open interpreter
  def eval2[T](v: DslElement[T]): T =
    v match {
      case op: Plus[T] =>
        op.tag match {
          case NumTag.Integer => eval2(op.a) + eval2(op.b)
          case NumTag.Dbl     => eval2(op.a) + eval2(op.b)
          case NumTag.Lng     => eval2(op.a) + eval2(op.b)
          case NumTag.Flt     => eval2(op.a) + eval2(op.b)
        }
      case op: Minus[T] =>
        op.tag match {
          case NumTag.Integer => eval2(op.a) - eval2(op.b)
          case NumTag.Dbl     => eval2(op.a) - eval2(op.b)
          case NumTag.Lng     => eval2(op.a) - eval2(op.b)
          case NumTag.Flt     => eval2(op.a) - eval2(op.b)
        }
      case op: Times[T] =>
        op.tag match {
          case NumTag.Integer => eval2(op.a) * eval2(op.b)
          case NumTag.Dbl     => eval2(op.a) * eval2(op.b)
          case NumTag.Lng     => eval2(op.a) * eval2(op.b)
          case NumTag.Flt     => eval2(op.a) * eval2(op.b)
        }
      case op: Negate[T] =>
        op.tag match {
          case NumTag.Integer => -eval2(v)
          case NumTag.Dbl     => -eval2(v)
          case NumTag.Lng     => -eval2(v)
          case NumTag.Flt     => -eval2(v)
        }
      case Literal(v, _) => v
    }

  try {
    // Statically typed DSL

    // val a = -(lit(1) + lit(7)) + lit(2)
    val b = -((1.0.lit + 7.0.lit) + 10.6.lit).as[Int] * 2.lit

    // 1.lit.d + 45.lit //could not find implicit value for parameter tag: net.demo.GADTs.NumTag[AnyVal]
    // 45.lit + 1.lit.d

    // TIMES|NEGATE|CAST|PLUS|PLUS|1:1.0|1:7.0|1:10.6->0|0:2
    println(serialize(b))
    println(eval(b))

    val exp = 12.lit.as[Double]
    println(serialize(exp))
    val r = eval(exp)

    println("out: > " + r)
  } catch {
    case NonFatal(ex) => ex.printStackTrace
  }

}
