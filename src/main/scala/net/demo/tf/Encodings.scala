package net.demo.tf

object Encodings extends App {

  type Id[A] = A

  sealed trait Ops[T]
  object Ops {
    final case class Plus(a: Ops[Int], b: Ops[Int])     extends Ops[Int]
    final case class Multiply(a: Ops[Int], b: Ops[Int]) extends Ops[Int]
    final case class Value(value: Int)                  extends Ops[Int]
  }

  trait Operators {
    type F[A]

    def add(a: F[Int], b: F[Int]): F[Int]

    def multiply(a: F[Int], b: F[Int]): F[Int]

    def value(value: Int): F[Int]
  }

  val executable = new Operators {
    type F[A] = Id[A]

    def add(a: Int, b: Int): Int = a + b

    def multiply(a: Int, b: Int): Int = a * b

    def value(value: Int): Int = value
  }

  val declarative = new Operators {
    type F[A] = Ops[A]

    def add(a: Ops[Int], b: Ops[Int]): Ops[Int] = Ops.Plus(a, b)

    def multiply(a: Ops[Int], b: Ops[Int]): Ops[Int] = Ops.Multiply(a, b)

    def value(value: Int) = Ops.Value(value)
  }

  val optimizedDeclarative = new Operators {
    type F[A] = Ops[A]

    def add(a: Ops[Int], b: Ops[Int]): Ops[Int] =
      (a, b) match {
        case (Ops.Value(a), Ops.Value(b)) => Ops.Value(a + b)
        case _                            => Ops.Plus(a, b)
      }

    def multiply(a: Ops[Int], b: Ops[Int]): Ops[Int] = Ops.Multiply(a, b)

    def value(value: Int) = Ops.Value(value)
  }

  final case class IntProgram(run: Operators => Operators#F[Int])

  val program = IntProgram { alg =>
    import alg._
    multiply(add(value(5), value(6)), value(10))
  }

  println("Executable: " + program.run(executable))
  println("Declarative: " + program.run(declarative))
  println("Optimized declarative: " + program.run(optimizedDeclarative))
}
