package net.demo

import net.demo.tf.Encodings
import scala.annotation.nowarn
import net.demo.MotivationDeclarative2.Numrc.Dbl
import net.demo.MotivationDeclarative2.Numrc.Lng
import net.demo.MotivationDeclarative2.CalculatedValue.Miltiple

/*
 * INTRODUCTION
 *
 * In Functional Design, type safety of data models can be greatly improved by
 * using so-called generalized algebraic data types.
 *
 * In this section, you'll review GADTs with a focus on functional models.
 *
 */

//GADTs - Parametricaly polimorpic ADT, where you are allowed to specialize a type parameter in the terms of the sum type.
//(compiler can know about type in each cases)

// Specializion the type T of Numrc inside the childer together with been able to reconsctuct this information in mater match if known as GADTs
/*
    def zero[T](N: Numrc[T]): T =
      N match {
        case Numrc.Integer ⇒ 1    //T =:= Int
        case Numrc.Dbl     ⇒ 1.0  //T =:= Double
        case Numrc.Lng     ⇒ 1L   //T =:= Long
        case Numrc.Flt     ⇒ 1.7f //T =:= Float
      }
 */

object Motivation {

  trait SpreadSheet

  sealed trait Value
  object Value {
    final case class Str(v: String)   extends Value
    final case class Dbl(v: Double)   extends Value
    final case class Err(err: String) extends Value
  }

  final case class CalculatedValue(eval: SpreadSheet => Value) { self =>
    def +(that: CalculatedValue): CalculatedValue =
      self.binaryOp(that)("Expected numeric value") {
        case (Value.Dbl(l), Value.Dbl(r)) => Value.Dbl(l + r)
        case (Value.Err(l), Value.Err(r)) => Value.Err(l + "\t" + r)
      }

    private def binaryOp(that: CalculatedValue)(error: String)(
      f: PartialFunction[(Value, Value), Value]
    ): CalculatedValue =
      CalculatedValue(s => f.lift((self.eval(s), that.eval(s))).getOrElse(Value.Err(error)))
  }

  object CalculatedValue {
    def const(v: Value) = CalculatedValue(_ => v)
  }

  import CalculatedValue._
  val a = const(Value.Str("a"))
  val b = const(Value.Str("a"))

  //What we have here as a dynamically typed dsl.
  //As a result, we have no idea if the result of a + b is valid or not. Scalac doesn't help here.
  //We need to turn your dynamically typed DSL to a statically typed.
  val s: SpreadSheet = ???

  (a + b).eval(s)

}

//Executable Statically typed DSL that works for all Numerics
object MotivationExecutable2 {

  trait SpreadSheet {
    def markLeft[T](v: T): T
    def markRight[T](v: T): T
  }

  final case class SpreadSheetImpl() extends SpreadSheet {

    def markLeft[T](v: T): T = {
      println(s"markLeft: $v")
      v
    }

    def markRight[T](v: T): T = {
      println(s"markRight: $v")
      v
    }
  }

  //Parametrically polymorphic ADT
  final case class CalculatedValue[+A](eval: SpreadSheet => A) { self =>
    //works only for ints
    //def +++(that: CalculatedValue[Int])(implicit ev: A <:< Int) = CalculatedValue(ss ⇒ ev(self.eval(ss)) + that.eval(ss))
    //works only for doubles
    //def ++(that: CalculatedValue[Double])(implicit ev: A <:< Double) = CalculatedValue(ss ⇒ ev(self.eval(ss)) + that.eval(ss))

    //if you're using declaration side covariance `final case class CalculatedValue[+A]` the "+"" sign
    //you going to have type bounds `def +[B >: A]` on all the methods that accept that as input.
    def +[B >: A](that: CalculatedValue[B])(implicit N: Numeric[B]): CalculatedValue[B] =
      CalculatedValue[B] { ss =>
        N.plus(self.eval(ss), that.eval(ss))
      }

    def -[B >: A](that: CalculatedValue[B])(implicit N: Numeric[B]): CalculatedValue[B] =
      CalculatedValue[B](ss => N.minus(self.eval(ss), that.eval(ss)))

    def unary_-[B >: A](implicit N: Numeric[B]): CalculatedValue[B] =
      CalculatedValue[B](ss => N.negate(self.eval(ss)))
  }

  object CalculatedValue {
    /*
    implicit class CalculatedValueDoubleOps(self: CalculatedValue[Double]) extends AnyVal {
      def ++(that: CalculatedValue[Double]): CalculatedValue[Double] = ???
    }
     */

    def v[A](v: A) = CalculatedValue(_ => v)

    def left[T](v: T) = CalculatedValue(ss => ss.markLeft(v))

    def right[T](v: T) = CalculatedValue(ss => ss.markRight(v))
  }

  import CalculatedValue._

  //val res = - v(9L) + v(4L)

  left(6) + right(67)

  left(6.0) + right(67.0)

  -(left(1) + right(7)) + left(2)

  (left(6) - right(67)).eval(SpreadSheetImpl())
}

object MotivationDeclarative2 {

  trait SpreadSheet {
    def markLeft[T](v: T): T
    def markRight[T](v: T): T
  }

  //we can't use scala.math.Numeric because it's not a sum nor product type
  sealed trait Numrc[T] { //self ⇒
    /*
    protected def plus[T: Numrc](a: T, b: T, N: Numrc[T]): T =
      N match {
        case Numrc.Dbl     ⇒ a + b
        case Numrc.Integer ⇒ a + b
        case Numrc.Lng     ⇒ a + b
        case Numrc.Flt     ⇒ a + b
      }

    protected def minus[T: Numrc](a: T, b: T, N: Numrc[T]): T =
      N match {
        case Numrc.Dbl     ⇒ a - b
        case Numrc.Integer ⇒ a - b
        case Numrc.Lng     ⇒ a - b
        case Numrc.Flt     ⇒ a - b
      }

    protected def neg[T: Numrc](a: T, N: Numrc[T]): T =
      N match {
        case Numrc.Integer ⇒ -a
        case Numrc.Dbl     ⇒ -a
        case Numrc.Lng     ⇒ -a
        case Numrc.Flt     ⇒ -a
      }

    def +(a: T, b: T)(implicit ev: Numrc[T]): T = plus[T](a, b, self /*ev*/ )
    def -(a: T, b: T)(implicit ev: Numrc[T]): T = minus[T](a, b, self /*ev*/ )
    def negate(a: T)(implicit ev: Numrc[T]): T  = neg[T](a, self)
     */

    //def match2(a: T, b: T)(isInt: (Int, Int) ⇒ Int, isLong: (Long, Long) ⇒ Long, isDbl: (Double, Double) ⇒ Double): T
  }

  object Numrc {
    //def match2(a: Int,b: Int)(isInt: (Int, Int) ⇒ Int, isLong: (Long, Long) ⇒ Long, isDbl: (Double, Double) ⇒ Double): Int =  isInt(a, b)
    implicit case object Integer extends Numrc[Int]
    implicit case object Dbl     extends Numrc[Double]
    implicit case object Flt     extends Numrc[Float]
    implicit case object Lng     extends Numrc[Long]
  }

  sealed trait CalculatedValue[+A] { self =>

    def +[B >: A](that: CalculatedValue[B])(implicit N: Numrc[B]): CalculatedValue[B] =
      CalculatedValue.Plus(self, that, N)

    def -[B >: A](that: CalculatedValue[B])(implicit N: Numrc[B]): CalculatedValue[B] =
      CalculatedValue.Minus(self, that, N)

    def *[B >: A](that: CalculatedValue[B])(implicit N: Numrc[B]): CalculatedValue[B] =
      CalculatedValue.Minus(self, that, N)

    def unary_-[B >: A](implicit N: Numrc[B]): CalculatedValue[B] =
      CalculatedValue.Negate(self, N)
  }

  object CalculatedValue {

    final case class Plus[A](a: CalculatedValue[A], b: CalculatedValue[A], N: Numrc[A])     extends CalculatedValue[A]
    final case class Minus[A](a: CalculatedValue[A], b: CalculatedValue[A], N: Numrc[A])    extends CalculatedValue[A]
    final case class Miltiple[A](a: CalculatedValue[A], b: CalculatedValue[A], N: Numrc[A]) extends CalculatedValue[A]
    final case class Negate[A](v: CalculatedValue[A], N: Numrc[A])                          extends CalculatedValue[A]
    final case class Const[A](v: A)                                                         extends CalculatedValue[A]

    def n[A](c: A): CalculatedValue[A] = Const(c)

    implicit class CalculatedValueOps[A](val self: A) extends AnyVal {
      def n(implicit ev: Numrc[A]): CalculatedValue[A] = CalculatedValue.Const(self)
    }
  }

  //def plus[A](a: A, b: A, n: Numrc[A]) = n.match2(a, b)(_ + _, _ + _, _ + _)
  /*
    n match {
      case Numrc.Integer ⇒ a + b
      case Numrc.Dbl     ⇒ a + b
      case Numrc.Lng     ⇒ a + b
    }
   */
  def eval[T](v: CalculatedValue[T]): T = {

    def plus[T](a: T, b: T, n: Numrc[T]): T =
      n match {
        case Numrc.Integer => a + b
        case Numrc.Dbl     => a + b
        case Numrc.Flt     => a + b
        case Numrc.Lng     => a + b
      }

    v match {
      case CalculatedValue.Const(v) => v
      /*
      This way pattern match DOESN'T work !!!!!!!!!
      case p: CalculatedValue.Plus(a, b, n) ⇒ n match { ... }
      But `case p: CalculatedValue.Plus[T] ⇒` works
       */

      case p: CalculatedValue.Plus[T] => plus(eval(p.a), eval(p.b), p.N)
      /*or
        p.N match {
          case Numrc.Integer ⇒ eval(p.a) + eval(p.b)
          case Numrc.Dbl     ⇒ eval(p.a) + eval(p.b)
          case Numrc.Lng     ⇒ eval(p.a) + eval(p.b)
          case Numrc.Flt     ⇒ eval(p.a) + eval(p.b)
        }
       */

      case p: CalculatedValue.Minus[T] =>
        //n.-(eval(a), eval(b))(n)
        p.N match {
          case Numrc.Integer => eval(p.a) - eval(p.b)
          case Numrc.Dbl     => eval(p.a) - eval(p.b)
          case Numrc.Lng     => eval(p.a) - eval(p.b)
          case Numrc.Flt     => eval(p.a) - eval(p.b)
        }

      case p: CalculatedValue.Negate[T] =>
        p.N match {
          case Numrc.Integer => -eval(p.v)
          case Numrc.Dbl     => -eval(p.v)
          case Numrc.Lng     => -eval(p.v)
          case Numrc.Flt     => -eval(p.v)
        }

      case p: CalculatedValue.Miltiple[T] =>
        p.N match {
          case Numrc.Integer => eval(p.a) * eval(p.b)
          case Numrc.Dbl     => eval(p.a) * eval(p.b)
          case Numrc.Lng     => eval(p.a) * eval(p.b)
          case Numrc.Flt     => eval(p.a) * eval(p.b)
        }
      //n.negate(eval(v))(n)
    }
  }

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

  import MotivationDeclarative2.CalculatedValue._

  //1.3.n + 2.4.n

  //"lsdf".n doen 't compile

  123.n
  val r0 = -(1.n + 7.n) + 2.n
  val r  = -(n(1) + n(7)) + n(2)

}

object MotivationExecutableEncoding {

  trait SpreadSheet

  /*sealed trait StrictValue
  object StrictValue {
    final case class Str(v: String)   extends StrictValue
    final case class Num(v: Double)   extends StrictValue
    final case class Err(err: String) extends StrictValue
  }*/

  //phantom types - don't exist in runtime
  //+ covariant
  case class CalculatedValue[+A](eval: SpreadSheet => A /*StrictValue*/ ) { self =>

    def +(that: CalculatedValue[Double])(implicit ev: A <:< Double): CalculatedValue[Double] =
      CalculatedValue { s =>
        self.as[Double].eval(s).+(that.eval(s))

      /*(self.run(s), that.run(s)) match {
          case (StrictValue.Num(a), StrictValue.Num(b)) => StrictValue.Num(a + b)
          case _                                        => StrictValue.Err("Boom !!!")
        }*/
      }

    private def as[B](implicit ev: A <:< B): CalculatedValue[B] =
      CalculatedValue(s => ev(self.eval(s)))

  }

  object Exts {
    implicit class Ops(self: CalculatedValue[Double]) {
      def add(that: CalculatedValue[Double]) =
        CalculatedValue(s => self.eval(s) + that.eval(s))
    }
  }

  val left: CalculatedValue[String]  = CalculatedValue(_ => "foo")
  val right: CalculatedValue[Double] = CalculatedValue(_ => .4)

  import Exts._
  //val sum0 = right add left

  //val sum1 = left + right //comp error

}

object MotivationDeclarativeEncoding {

  //GADT
  /*
  sealed trait StrictValue[+A]
  object StrictValue {
    final case class Str(v: String)   extends StrictValue[String]
    final case class Num(v: Double)   extends StrictValue[Double]
    final case class Err(err: String) extends StrictValue[Nothing]
  }
   */

  trait SpreadSheet

  /*sealed trait StrictValue
  object StrictValue {
    final case class Str(v: String)   extends StrictValue
    final case class Num(v: Double)   extends StrictValue
    final case class Err(err: String) extends StrictValue
  }*/

  final case class Coercion[-A, +B](coerce: A => Option[B])
  object Coercion {
    implicit val IntToDouble = Coercion[Int, Double](int => Some(int.toDouble))
  }

  //phantom types - don't exist in runtime
  //+ covariant
  sealed trait CalculatedValue[+A] { self =>

    def +(that: CalculatedValue[Double])(implicit ev: A <:< Double): CalculatedValue[Double] =
      CalculatedValue.Add(self.as[Double], that)

    def as[B](implicit ev: A <:< B): CalculatedValue[B] =
      CalculatedValue.As(self, ev)

    def coerce[B](implicit coercion: Coercion[A, B]): CalculatedValue[B] =
      CalculatedValue.Coerce(self, coercion)
  }

  object CalculatedValue {

    case class Constant[A](value: A) extends CalculatedValue[A]

    case class Add(left: CalculatedValue[Double], right: CalculatedValue[Double]) extends CalculatedValue[Double]

    case class As[A, B](value: CalculatedValue[A], ev: A <:< B) extends CalculatedValue[B]

    final case class Coerce[A, B](value: CalculatedValue[A], coercion: Coercion[A, B]) extends CalculatedValue[B]

    def str(v: String): CalculatedValue[String] = Constant(v)
    def dbl(v: Double): CalculatedValue[Double] = Constant(v)

  }

  def eval[A](sheet: SpreadSheet, value: CalculatedValue[A]): A =
    value match {
      case CalculatedValue.Constant(v) => v
      case CalculatedValue.Coerce(v, coercion) =>
        coercion
          .coerce(eval(sheet, v))
          .getOrElse(throw new IllegalStateException("Couldn't not convert  !"))

      case CalculatedValue.Add(l, r) =>
        //#1: value: CalculatedValue[A]
        //#2: value: CalculatedValue[Double]
        //#3: CalculatedValue[Double] <:< CalculatedValue[A]
        //#4: CalculatedValue[A] =:= CalculatedValue[Double] ===> A =:= Double

        //42.0
        eval(sheet, l) + eval(sheet, r)
      case CalculatedValue.As(a, ev) => ev(eval(sheet, a))
    }

  val left: CalculatedValue[String]  = ???
  val right: CalculatedValue[Double] = ???

  //val sum = left + right

}

/** EXPRESSIONS - EXERCISE SET 1
  *
  * Consider an application (such as the spreadsheet example) that needs to
  * calculate values in a user-defined way.
  */

object expr0 {

  //type-level function
  sealed trait Adder[A] {
    def add(a: A, b: A): A
  }

  object Adder {
    implicit val AdderInt: Adder[Int]       = new Adder[Int] { def add(l: Int, r: Int): Int = l + r }
    implicit val AdderString: Adder[String] = new Adder[String] { def add(l: String, r: String): String = l + r }
  }

  sealed trait CalculatedValue[+A] { self =>
    def +[B >: A](that: CalculatedValue[B])(implicit adder: Adder[B]): CalculatedValue[B] =
      CalculatedValue.Add(self, that, adder)
  }

  object CalculatedValue {

    final case class Integer[A](value: A)                                                  extends CalculatedValue[A]
    final case class Str[A](value: A)                                                      extends CalculatedValue[A]
    final case class Add[A](a: CalculatedValue[A], b: CalculatedValue[A], adder: Adder[A]) extends CalculatedValue[A]
  }

  def calculate[A](expr: CalculatedValue[A]): A =
    expr match {
      case CalculatedValue.Integer(v) => v
      case CalculatedValue.Str(v)     => v
      case op: CalculatedValue.Add[A] =>
        val a = calculate(op.a)
        val b = calculate(op.b)
        op.adder.add(a, b)
    }

  val exp = CalculatedValue.Integer(4) + CalculatedValue.Str(89)
  calculate(exp)

}

object expr {

  sealed trait CalculatedValue[+A]
  object CalculatedValue {

    final case class Integer(value: Int) extends CalculatedValue[Int]
    final case class Str(value: String)  extends CalculatedValue[String]

    /** EXERCISE 1
      *
      * Add an operator that adds two integer expressions, yielding an integer
      * expression.
      *
      * NOTE: Be sure to modify the `calculate` method below, so that it can
      * handle the new operation.
      */

    final case class Add(a: Integer, b: Integer) extends CalculatedValue[Int]

    /** EXERCISE 2
      *
      * Add an operator that subtracts an integer from another integer expression,
      * yielding an integer expression.
      *
      * NOTE: Be sure to modify the `calculate` method below, so that it can
      * handle the new operation.
      */
    final case class Subtract(a: Integer, b: Integer) extends CalculatedValue[Int]

    /** EXERCISE 3
      *
      * Add an operator that multiplies two integer expressions, yielding an
      * integer expression.
      *
      * NOTE: Be sure to modify the `calculate` method below, so that it can
      * handle the new operation.
      */
    final case class Multiply(a: Integer, b: Integer) extends CalculatedValue[Int]

    /** EXERCISE 4
      *
      * Add an operator that concatenates two strings, yielding a string
      * expression.
      *
      * NOTE: Be sure to modify the `calculate` method below, so that it can
      * handle the new operation.
      */
    final case class Concat(a: Str, b: Str) extends CalculatedValue[String]

    /** EXERCISE 5
      *
      * Add an operator that determines if a string starts with a specified
      * prefix, yielding a boolean expression.
      *
      * NOTE: Be sure to modify the `calculate` method below, so that it can
      * handle the new operation.
      */
    final case class StartsWith(prefix: Str, value: Str) extends CalculatedValue[Boolean]
  }

  import CalculatedValue._

  def calculate[A](expr: CalculatedValue[A]): A =
    expr match {
      case Integer(v)          => v
      case Str(v)              => v
      case Add(a, b)           => calculate(a) + calculate(b)
      case Subtract(a, b)      => calculate(a) - calculate(b)
      case Multiply(a, b)      => calculate(a) * calculate(b)
      case Concat(a, b)        => calculate(a).concat(calculate(b))
      case StartsWith(pref, a) => calculate(pref).startsWith(calculate(a))
    }
}

/** PARSERS - EXERCISE SET 2
  */
object parser {
  type Error = String
  type Input = String

  final case class ParserExecutable[+A](parse: Input => Either[Error, (Input, A)])

  // `Parser[A]` is a model of a series of parse operations that consume
  // characters and ultimately use the consumed input construct a value of
  // type `A`.
  sealed trait Parser[+A] { self =>
    def atLeast(n: Int): Parser[List[A]] = Parser.Repeat(self, Some(n), None)

    def atMost(n: Int): Parser[List[A]] = Parser.Repeat(self, None, Some(n))

    def between(min: Int, max: Int): Parser[List[A]] = Parser.Repeat(self, Some(min), Some(max))

    def repeat: Parser[List[A]] = Parser.Repeat(self, None, None)
  }

  object Parser {
    final case object OneChar extends Parser[Char]

    final case class Repeat[A](value: Parser[A], min: Option[Int], max: Option[Int]) extends Parser[List[A]]

    /** EXERCISE 1
      *
      * Add a constructor that models the production of the specified value (of
      * any type at all), without consuming any input.
      *
      * NOTE: Be sure to modify the `parse` method below, so that it can
      * handle the new operation.
      */
    final case class Succeed[A](value: A) extends Parser[A]

    /** EXERCISE 2
      *
      * Add a constructor that models failure with a string error message.
      *
      * NOTE: Be sure to modify the `parse` method below, so that it can
      * handle the new operation.
      */
    final case class Fail[A](err: Error) extends Parser[Nothing]

    /** EXERCISE 3
      *
      * Add an operator that can try one parser, but if that fails, try
      * another parser.
      *
      * NOTE: Be sure to modify the `parse` method below, so that it can
      * handle the new operation.
      */
    final case class OrElse[A](p: Parser[A], fallback: Parser[A]) extends Parser[A]

    /** EXERCISE 4
      *
      * Add an operator that parses one thing, and then parses another one,
      * in sequence, producing a tuple of their results.
      *
      * NOTE: Be sure to modify the `parse` method below, so that it can
      * handle the new operation.
      */
    final case class Sequence[A, B](a: Parser[A], b: Parser[A]) extends Parser[(A, B)]
  }

  import Parser._

  def parse[A](parser: Parser[A], input: Input): Either[Error, (Input, A)] =
    parser match {
      case OneChar =>
        input.headOption
          .map((a: Char) => Right(input.drop(1) -> a))
          .getOrElse(Left("Expected a character but found an empty string"))

      case repeat: Repeat[a] =>
        val min = repeat.min.getOrElse(0)
        val max = repeat.max.getOrElse(Int.MaxValue)

        (min to max)
          .foldLeft[Either[Error, (Input, List[a])]](Right((input, Nil))) {
            case (e @ Left(_), _) => e

            case (Right((input, as)), _) =>
              parse[a](repeat.value, input) match {
                case Left(error)       => if (as.length >= min) Right((input, as)) else Left(error)
                case Right((input, a)) => Right((input, a :: as))
              }
          }
          .map { case (input, as) =>
            (input, as.reverse)
          }

      case Succeed(value) =>
        Right((input, value))
      case Fail(err) =>
        Left(err)
      case OrElse(p, fallback) =>
        parse(p, input) match {
          case Left(_) => parse(fallback, input)
          case x       => x
        }
      case Sequence(l, r) =>
        for {
          r1 <- parse(l, input)
          r2 <- parse(r, r1._1)
        } yield (r2._1, (r1._2, r2._2))
    }
}
