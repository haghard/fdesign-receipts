package net.demo

import scala.util.Try
import scala.util.control.NonFatal
import scala.util.{Failure, Success}
import net.demo.GADTs.Num.Dbl
import net.demo.GADTs.Num.Flt
import net.demo.GADTs.Num.Lng

/** GADTs - Parametricaly polimorpic ADT, where you are allowed to specialize a type parameter in the terms of the sum type.
  *
  * Specializion the type T of Num inside the childer together with being able to reconsctuct this information in pattern matching is known as GADTs.
  *
  * //(Scalac knows about types in each cases)
  * def zero[T](N: Num[T]): T =
  * N match {
  * case Num.Integer ⇒ 1    //T =:= Int
  * case Num.Dbl     ⇒ 1.0  //T =:= Double
  * case Num.Lng     ⇒ 1L   //T =:= Long
  * case Num.Flt     ⇒ 1.7f //T =:= Float
  * }
  *
  * runMain net.demo.GADTs
  */
object GADTs extends App {

  sealed trait SerializableVal[T]

  object SerializableVal {
    implicit case object Integer extends SerializableVal[Int]

    implicit case object Dbl extends SerializableVal[Double]

    implicit case object Flt extends SerializableVal[Float]

    implicit case object Lng extends SerializableVal[Long]
  }

  //Declarative encoding
  //We turn our dynamically typed model of CalculatedValue into a statically typed DSL
  //we can't use scala.math.Numeric because it's not a sum nor a product type.
  //As we use decrarative encoding we want to have as many interpreters as we'd like.
  sealed trait Num[T]

  object Num {
    implicit case object Integer extends Num[Int]

    implicit case object Dbl extends Num[Double]

    implicit case object Flt extends Num[Float]

    implicit case object Lng extends Num[Long]
  }

  sealed abstract case class Cast[-In, +Out](op: In => Try[Out])

  //A set of casts this DSL supports
  object Cast {
    implicit object A extends Cast[Int, Double]((in: Int) => Try(in.toDouble))

    implicit object B extends Cast[String, Double]((in: String) => Try(in.toDouble))

    implicit object C extends Cast[Double, Int]((in: Double) => Try(in.toInt))
  }

  import Cast._

  /*
  sealed trait Coercion[In, Out] {
    def from: Num[In]

    def to: Num[Out]

    def toSer: SerializableVal[Out]
  }

  object Coercion {

    implicit object IntToDouble extends Coercion[Int, Double] {
      def from = Num.Integer

      def to = Num.Dbl

      def toSer = SerializableVal.Dbl
    }

    implicit object DoubleToInt extends Coercion[Double, Int] {
      def from = Num.Dbl

      def to = Num.Integer

      def toSer = SerializableVal.Integer
    }

    implicit object LongToInt extends Coercion[Long, Int] {
      def from = Num.Lng

      def to = Num.Integer

      def toSer = SerializableVal.Integer
    }
  }*/

  //Functional Scala - Next-Level Type Safety: An Intro to Generalized Algebraic Data Types https://youtu.be/1E053kld8Ac?t=331
  /*
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

  sealed trait CalculatedValue[+A] {
    self =>

    def +[B >: A](that: CalculatedValue[B])(implicit N: Num[B]): CalculatedValue[B] =
      CalculatedValue.Plus(self, that, N)

    def -[B >: A](that: CalculatedValue[B])(implicit N: Num[B]): CalculatedValue[B] =
      CalculatedValue.Minus(self, that, N)

    def *[B >: A](that: CalculatedValue[B])(implicit N: Num[B]): CalculatedValue[B] =
      CalculatedValue.Times(self, that, N)

    def as[B](implicit cast: Cast[A, B], to: Num[B]): CalculatedValue[B] =
      CalculatedValue.CastTo(self, cast, to)

    def unary_-[B >: A](implicit N: Num[B]): CalculatedValue[B] =
      CalculatedValue.Negate(self, N)

    /*
    def +(that: CalculatedValue[A])(implicit N: Num[A]): CalculatedValue[A] =
      CalculatedValue.Plus(self, that, N)

    def -(that: CalculatedValue[A])(implicit N: Num[A]): CalculatedValue[A] =
      CalculatedValue.Minus(self, that, N)

    def *(that: CalculatedValue[A])(implicit N: Num[A]): CalculatedValue[A] =
      CalculatedValue.Times(self, that, N)

    def as[B](implicit coercion: Coercion[A, B], N: Num[B]): CalculatedValue[B] =
      CalculatedValue.Coercible(self, coercion)

    def unary_-(implicit N: Num[A]): CalculatedValue[A] =
      CalculatedValue.Negate(self, N)*/
  }

  object CalculatedValue {

    final case class Plus[A](a: CalculatedValue[A], b: CalculatedValue[A], N: Num[A]) extends CalculatedValue[A]

    final case class Minus[A](a: CalculatedValue[A], b: CalculatedValue[A], N: Num[A]) extends CalculatedValue[A]

    final case class Times[A](a: CalculatedValue[A], b: CalculatedValue[A], N: Num[A]) extends CalculatedValue[A]

    final case class Negate[A](v: CalculatedValue[A], N: Num[A]) extends CalculatedValue[A]

    final case class Literal[A](v: A, sv: SerializableVal[A]) extends CalculatedValue[A]

    final case class CastTo[A, B](v: CalculatedValue[A], cast: Cast[A, B], N: Num[B]) extends CalculatedValue[B]
    //final case class Coerce[A, B](v: CalculatedValue[A], c: Coercion[A, B])            extends CalculatedValue[B]

    /*
    final case class IntLiteral[A](v: CalculatedValue[A], aIsInt: TypeEquality[A, Int])    extends CalculatedValue[A]
    final case class DblLiteral[A](v: CalculatedValue[A], aIsInt: TypeEquality[A, Double]) extends CalculatedValue[A]
    final case class FltLiteral[A](v: CalculatedValue[A], aIsInt: TypeEquality[A, Float])  extends CalculatedValue[A]
    final case class LongLiteral[A](v: CalculatedValue[A], aIsInt: TypeEquality[A, Long])  extends CalculatedValue[A]
     */

    def lit[A](c: A)(implicit ev: Num[A], cst: SerializableVal[A]): CalculatedValue[A] =
      Literal(c, cst)

    implicit class CalculatedValueOps[A](val self: A) extends AnyVal {
      def lit(implicit ev: Num[A], sv: SerializableVal[A]): CalculatedValue[A] =
        Literal(self, sv)
    }

    implicit class CalculatedValueStrOps(val self: String) extends AnyVal {
      def fromStr[A](implicit
        ev: Num[A],
        sv: SerializableVal[A]
      ): CalculatedValue[A] =
        ev match {
          case Num.Integer => Try(self.toInt).map(Literal(_, sv)).getOrElse(throw new Exception("toInt !!!"))
          case Num.Dbl     => Try(self.toDouble).map(Literal(_, sv)).getOrElse(throw new Exception("toDouble !!!"))
          case Num.Flt     => Try(self.toFloat).map(Literal(_, sv)).getOrElse(throw new Exception("toFloat !!!"))
          case Num.Lng     => Try(self.toLong).map(Literal(_, sv)).getOrElse(throw new Exception("toLong !!!"))
        }
      //CalculatedValue.Literal(ev.fromStr(self)(_.toInt, _.toDouble, _.toFloat, _.toLong), sv)
    }
  }

  import Num._
  import CalculatedValue._

  //default interpreter
  def eval[T](v: CalculatedValue[T]): T = {
    def plus[T](a: T, b: T, n: Num[T]): T =
      n match {
        case Integer => a + b
        case Dbl     => a + b
        case Flt     => a + b
        case Lng     => a + b
      }

    def minus[T](a: T, b: T, n: Num[T]): T =
      n match {
        case Integer => a - b
        case Dbl     => a - b
        case Flt     => a - b
        case Lng     => a - b
      }

    def times[T](a: T, b: T, n: Num[T]): T =
      n match {
        case Integer => a * b
        case Dbl     => a * b
        case Flt     => a * b
        case Lng     => a * b
      }

    def negate[T](a: T, n: Num[T]): T =
      n match {
        case Integer => -a
        case Dbl     => -a
        case Flt     => -a
        case Lng     => -a
      }

    /*def converter[A, B](from: Num[A], to: Num[B]): A => B =
      (from, to) match {
        case (Integer, Dbl) => in: Int => in.toDouble
        case (Integer, Lng) => in: Int => in.toLong
        case (Integer, Flt) => in: Int => in.toFloat

        case (Dbl, Integer) => in: Double => in.toInt
        case (Dbl, Lng)     => in: Double => in.toLong
        case (Dbl, Flt)     => in: Double => in.toFloat

        case (Flt, Integer) => in: Float => in.toInt
        case (Flt, Dbl)     => in: Float => in.toDouble
        case (Flt, Lng)     => in: Float => in.toLong

        case (Lng, Integer) => in: Long => in.toInt
        case (Lng, Dbl)     => in: Long => in.toDouble
        case (Lng, Flt)     => in: Long => in.toFloat

        case _ => ???
      }*/

    v match {
      /*
        [error]  found   : net.demo.GADTs.Num[?T2] where type ?T2 <: T (this is a GADT skolem)
        [error]  required: net.demo.GADTs.Num[T]
        case Plus(a, b, n) ⇒ plus(eval(a), eval(b), n)
       */
      case op: Plus[T] =>
        plus(eval(op.a), eval(op.b), op.N)
      case op: Minus[T] =>
        minus(eval(op.a), eval(op.b), op.N)
      case op: Times[T] =>
        times(eval(op.a), eval(op.b), op.N)
      case op: Negate[T] =>
        negate(eval(op.v), op.N)
      case Literal(v, _) => v
      case castOp: CastTo[in, out] =>
        val rawValue = eval(castOp.v)
        castOp.cast.op(rawValue) match {
          case Success(v) => v
          case Failure(ex) =>
            throw new IllegalArgumentException(
              s"Cast error: ${rawValue.getClass.getName} -> ${castOp.N.getClass.getName}",
              ex
            )
        }
      /*case coerce: Coerce[in, out] =>
        val rawValue = eval(coerce.v) //.asInstanceOf[coerce.c.In]
        converter(coerce.c.from, coerce.c.to)(rawValue)*/
    }
  }

  val i: Byte = 0x00
  val d: Byte = 0x01
  val f: Byte = 0x02
  val l: Byte = 0x03

  def serialize[T](v: CalculatedValue[T]): String =
    v match {
      case Plus(a, b, n)  => s"PLUS|" + serialize(a) + "|" + serialize(b)
      case Minus(a, b, n) => s"MINUS|" + serialize(a) + "|" + serialize(b)
      case Times(a, b, n) => s"TIMES|" + serialize(a) + "|" + serialize(b)
      case Negate(v, n)   => s"NEGATE|" + serialize(v)
      case Literal(v, c) =>
        c match {
          case SerializableVal.Integer => s"$i:$v"
          case SerializableVal.Dbl     => s"$d:$v"
          case SerializableVal.Flt     => s"$f:$v"
          case SerializableVal.Lng     => s"$l:$v"
        }
      case c: CastTo[in, out] =>
        val to = c.N match {
          case Num.Integer => i
          case Num.Dbl     => d
          case Num.Flt     => f
          case Num.Lng     => l
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

  def deserialize(line: String): CalculatedValue[_] = ???

  //another open interpteter
  /*
  def eval2[T](v: CalculatedValue[T]): T =
    v match {
      case CalculatedValue.Const(v) ⇒ v
      case CalculatedValue.Plus(a, b, n) ⇒
        n match {
          case Numrc.Integer ⇒ (eval(a) + eval(b)) * 2
          case Numrc.Dbl     ⇒ (eval(a) + eval(b)) * 2
          case Numrc.Lng     ⇒ (eval(a) + eval(b)) * 2
          case Numrc.Flt     ⇒ (eval(a) + eval(b)) * 2
        }
      //n.+(eval(a), eval(b))(n)
      case CalculatedValue.Minus(a, b, n) ⇒
        n match {
          case Numrc.Integer ⇒ (eval(a) - eval(b)) * 2
          case Numrc.Dbl     ⇒ (eval(a) - eval(b)) * 2
          case Numrc.Lng     ⇒ (eval(a) - eval(b)) * 2
          case Numrc.Flt     ⇒ (eval(a) - eval(b)) * 2
        }
      case Miltiple(a, b, n) ⇒
        n match {
          case Numrc.Integer ⇒ (eval(a) * eval(b)) * 2
          case Numrc.Dbl     ⇒ (eval(a) * eval(b)) * 2
          case Numrc.Lng     ⇒ (eval(a) * eval(b)) * 2
          case Numrc.Flt     ⇒ (eval(a) * eval(b)) * 2
        }
      case CalculatedValue.Negate(v, n) ⇒
        n match {
          case Numrc.Integer ⇒ -eval(v) * 2
          case Numrc.Dbl     ⇒ -eval(v) * 2
          case Numrc.Lng     ⇒ -eval(v) * 2
          case Numrc.Flt     ⇒ -eval(v) * 2
        }
    }
   */

  try {
    //Statically typed DSL
    val a = -(lit(1) + lit(7)) + lit(2)
    val b = -((1.0.lit + 7.0.lit) + 10.6.lit).as[Int] * 2.lit

    //TIMES|NEGATE|CAST|PLUS|PLUS|1:1.0|1:7.0|1:10.6->0|0:2
    println(serialize(b))

    val exp = 12.lit.as[Double]
    println(serialize(exp))
    val r = eval(exp)

    println("out: > " + r)
  } catch {
    case NonFatal(ex) => ex.printStackTrace
  }

}
