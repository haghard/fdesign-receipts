package net.demo

import scala.util.control.NonFatal
import scala.util.Try
import scala.util.Failure
import scala.util.Success

object Starter extends App {

  final case class Coercion[-A, +B](coerce: A => Try[B]) extends AnyVal
  object Coercion {
    implicit val IntToDouble = Coercion[Int, Double](int => Try(int.toDouble))
    implicit val StrToInt    = Coercion[String, Int](str => Try(str.toInt))
    implicit val StrToDouble = Coercion[String, Double](str => Try(str.toDouble))
  }

  //Declarative encoding
  //CalculatedValue that works only with doubles
  sealed trait CalculatedValue[+A] { self =>

    def +(that: CalculatedValue[Double])(implicit ev: A <:< Double): CalculatedValue[Double] =
      CalculatedValue.Both(self.as[Double], that)

    def coerce[B](implicit coercion: Coercion[A, B]): CalculatedValue[B] =
      CalculatedValue.Coerce(self, coercion)

    private def as[B](implicit ev: A <:< B): CalculatedValue[B] =
      CalculatedValue.As(self, ev)
  }

  object CalculatedValue {
    final case class Const[A](value: A)                                                  extends CalculatedValue[A]
    final case class Both(left: CalculatedValue[Double], right: CalculatedValue[Double]) extends CalculatedValue[Double]
    final case class As[A, B](value: CalculatedValue[A], ev: A <:< B)                    extends CalculatedValue[B]
    final case class Coerce[A, B](value: CalculatedValue[A], coercion: Coercion[A, B])   extends CalculatedValue[B]

    def n(v: String): CalculatedValue[String]       = Const(v)
    def lit(value: Double): CalculatedValue[Double] = Const(value)

    implicit class CalculatedValueDoubleOps(val self: Double) extends AnyVal {
      def n = Const(self)
    }

    implicit class CalculatedValueStingOps(val self: String) extends AnyVal {
      def parse = Const(self)
    }
  }

  def eval[A](value: CalculatedValue[A]): A =
    value match {
      case CalculatedValue.Const(v) =>
        v
      case CalculatedValue.Coerce(v, coercion) =>
        coercion.coerce(eval(v)) match {
          case Failure(ex) => throw new IllegalArgumentException("Couldn't apply coercion", ex)
          case Success(v)  => v
        }
      case CalculatedValue.Both(l, r) =>
        eval(l) + eval(r)
      case CalculatedValue.As(a, ev) =>
        ev(eval(a))
    }

  import CalculatedValue._

  val strA = "56.02".parse //str("56.01a")
  val numA = 4.8.n         //v(4.8)
  val numB = 42.90.n       //v(42.90)

  import Coercion._
  val exp = numA + numB + strA.coerce[Double]

  try {
    val r = eval(exp)
    println(s"out > $r")
  } catch {
    case NonFatal(ex) =>
      println("Error !!!")
      ex.printStackTrace
  }
}
