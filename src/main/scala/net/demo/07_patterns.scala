package net.demo

import net.demo.parser.Parser

import scala.concurrent.Future

/*
 * INTRODUCTION
 *
 * In Functional Design, operators that transform and compose values in a
 * domain often fall into pre-existing patterns.
 *
 * In this section, you'll learn to identify these patterns.
 *
 */

/** BINARY COMPOSITION PATTERNS FOR VALUES - EXERCISE SET 1
  */
object binary_values {

  trait Quiz { def +(that: Quiz): Quiz }

  val q1, q2, q3: Quiz = ???

  (q1 + q2) + q3
  q1 + (q2 + q3)

  object Exercise1 {

    /** EXERCISE 1
      *
      * Choose a type such that you can implement the `compose` function in
      * such a way that:
      *
      * {{{
      * compose(compose(a, b), c) == compose(a, compose(b, c))
      * }}}
      *
      * for all `a`, `b`, `c`.
      */

    type SomeType = Int

    def compose(left: SomeType, right: SomeType): SomeType = left + right
  }

  object Exercise2 {

    /** EXERCISE 2
      *
      * Choose a different type such that you can implement the `compose`
      * function in such a way that:
      *
      * {{{
      * compose(compose(a, b), c) == compose(a, compose(b, c))
      * }}}
      *
      * for all `a`, `b`, `c`.
      */
    type SomeType

    def compose(left: SomeType, right: SomeType): SomeType = ???
  }

  object Exercise3 {

    /** EXERCISE 3
      *
      * Choose a type such that you can implement the `compose`
      * function in such a way that:
      *
      * {{{
      * compose(a, b) == compose(b, a)
      * }}}
      *
      * for all `a`, `b`.
      */

    type SomeType[A] = Option[A]
    // combine is commutative =>
    def compose[A](left: SomeType[A], right: SomeType[A])(combine: (A, A) => A): SomeType[A] =
      for {
        a <- left
        b <- right
      } yield combine(a, b)

    //type SomeType

    //def compose(left: SomeType, right: SomeType): SomeType = ???
  }

  object Exercise4 {

    /** EXERCISE 4
      *
      * Choose a different type such that you can implement the `compose`
      * function in such a way that:
      *
      * {{{
      * compose(a, b) == compose(b, a)
      * }}}
      *
      * for all `a`, `b`.
      */
    type SomeType

    def compose(left: SomeType, right: SomeType): SomeType = ???
  }

  object Exercise5 {

    /** EXERCISE 5
      *
      * Choose or create a data type such that your implementation
      * of `compose` represents modeling "both". For example, if you have
      * a data type that represents a query, then this `compose` could
      * combine two queries into one query, such that both results would
      * be queried when the model is executed.
      */
    type SomeType[A] = zio.UIO[A]
    def compose[A](left: SomeType[A], right: SomeType[A]): SomeType[A] = left.flatMap(_ => right)
  }

  object Exercise6 {

    /** EXERCISE 6
      *
      * Choose or create a different type such that your implementation
      * of `compose` represents modeling "both".
      */
    type SomeType

    def compose(left: SomeType, right: SomeType): SomeType = ???
  }

  object Exercise7 {

    /** EXERCISE 7
      *
      * Choose or create a data type such that your implementation
      * of `compose` represents modeling "or". For example, if you have
      * a data type that represents a query, then this `compose` could
      * model running one query, but if it fails, running another.
      */
    type SomeType

    def compose(left: SomeType, right: SomeType): SomeType = ???
  }

  object Exercise8 {

    /** EXERCISE 8
      *
      * Choose or create a different type such that your implementation
      * of `compose` represents modeling "or".
      */
    type SomeType

    def compose(left: SomeType, right: SomeType): SomeType = ???
  }

  object Exercise9 {

    /** EXERCISE 9
      *
      * Choose a type and a value called `identity` such that you can implement
      * the `compose` function in such a way that:
      *
      * {{{
      * compose(a, identity) == compose(identity, a) == a
      * }}}
      *
      * for all `a`.
      */
    type SomeType = Int

    def identity: SomeType = 0

    def compose(left: SomeType, right: SomeType): SomeType = left + right
  }

  object Exercise10 {

    /** EXERCISE 10
      *
      * Choose a different type and a value called `identity` such that you can
      * implement the `compose` function in such a way that:
      *
      * {{{
      * compose(a, identity) == compose(identity, a) == a
      * }}}
      *
      * for all `a`.
      */
    type SomeType = String

    def identity: SomeType = ""

    def compose(left: SomeType, right: SomeType): SomeType = left + right
  }
}

/** BINARY COMPOSITION PATTERNS FOR TYPE CONSTRUCTORS - EXERCISE SET 2
  */
object binary_tcs {
  object Exercise1 {

    /** EXERCISE 1
      *
      * Choose a type such that you can implement the `compose` function in
      * such a way that:
      *
      * {{{
      * compose(compose(a, b), c) ~ compose(a, compose(b, c))
      * }}}
      *
      * for all `a`, `b`, `c`, where `~` means "equivalent to".
      */
    type SomeType[A] = Option[A]
    //Future[A]
    //List[A]
    //(a zip b) zip c === a zip (c zip c)

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[(A, B)] =
      for { a <- left; b <- right } yield (a, b)
    //left zip right
  }

  object Exercise2 {

    /** EXERCISE 2
      *
      * Choose a different type such that you can implement the `compose` function
      * in such a way that:
      *
      * {{{
      * compose(compose(a, b), c) ~ compose(a, compose(b, c))
      * }}}
      *
      * for all `a`, `b`, `c`, where `~` means "equivalent to".
      */
    type SomeType[A] = zio.Task[A]
    //Option[A]

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[(A, B)] =
      for { a <- left; b <- right } yield (a, b)
  }

  //Tuple and Either
  object Exercise3 {

    /** EXERCISE 3
      *
      * Choose a type such that you can implement the `compose` function in
      * such a way that:
      *
      * {{{
      * compose(compose(a, b), c) ~ compose(a, compose(b, c))
      * }}}
      *
      * for all `a`, `b`, `c`, where `~` means "equivalent to".
      */
    type SomeType[A] = zio.UIO[A]

    //def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[Either[A, B]] = ???

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[Either[A, B]] =
      left raceEither right

  }

  object Exercise4 {

    /** EXERCISE 4
      *
      * Choose a different type such that you can implement the `compose` function
      * in such a way that:
      *
      * {{{
      * compose(compose(a, b), c) ~ compose(a, compose(b, c))
      * }}}
      *
      * for all `a`, `b`, `c`, where `~` means "equivalent to".
      */
    type SomeType[A] = zio.Task[A]

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[Either[A, B]] = ??? //left.zip(right)
  }

  object Exercise5 {

    /** EXERCISE 5
      *
      * Choose a type such that you can implement the `compose` function in
      * such a way that:
      *
      * {{{
      * compose(a, b) ~ compose(b, a)
      * }}}
      *
      * for all `a`, `b`, where `~` means "equivalent to".
      */
    type SomeType[A]

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[(A, B)] = ???
  }

  object Exercise6 {

    /** EXERCISE 6
      *
      * Choose a different type such that you can implement the `compose` function
      * in such a way that:
      *
      * {{{
      * compose(a, b) ~ compose(b, a)
      * }}}
      *
      * for all `a`, `b`, where `~` means "equivalent to".
      */
    type SomeType[A]

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[(A, B)] = ???
  }

  object Exercise7 {

    /** EXERCISE 7
      *
      * Choose a type such that you can implement the `compose` function in
      * such a way that:
      *
      * {{{
      * compose(a, b) ~ compose(b, a)
      * }}}
      *
      * for all `a`, `b`, where `~` means "equivalent to".
      */
    type SomeType[A] = Set[A]
    //zio.Task[A]

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[Either[A, B]] =
      left.map(Left(_)) ++ right.map(Right(_))
    //left.raceEither(right)
  }

  object Exercise8 {

    /** EXERCISE 8
      *
      * Choose a different type such that you can implement the `compose` function
      * in such a way that:
      *
      * {{{
      * compose(a, b) ~ compose(b, a)
      * }}}
      *
      * for all `a`, `b`, where `~` means "equivalent to".
      */
    type SomeType[A]

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[Either[A, B]] = ???
  }

  object Exercise9 {

    /** EXERCISE 9
      *
      * Choose or create a data type such that your implementation
      * of `compose` represents modeling "both". For example, if you have
      * a data type that represents a query, then this `compose` could
      * combine two queries into one query, such that both results would
      * be queried when the model is executed.
      */
    type SomeType[A]

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[(A, B)] = ???
  }

  object Exercise10 {

    /** EXERCISE 10
      *
      * Choose or create a different type such that your implementation
      * of `compose` represents modeling "both".
      */
    type SomeType[A]

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[(A, B)] = ???
  }

  object Exercise11 {

    /** EXERCISE 11
      *
      * Choose or create a data type such that your implementation
      * of `compose` represents modeling "or". For example, if you have
      * a data type that represents a query, then this `compose` could
      * model running one query, but if it fails, running another.
      */
    type SomeType[A]

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[Either[A, B]] = ???
  }

  object Exercise12 {

    /** EXERCISE 12
      *
      * Choose or create a different type such that your implementation
      * of `compose` represents modeling "or".
      */
    type SomeType[A]

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[Either[A, B]] = ???
  }

  object Exercise13 {

    /** EXERCISE 13
      *
      * Choose or create a type `SomeType` and a value called `identity` such
      * that you can implement the `compose` function in such a way that:
      *
      * {{{
      * compose(a, identity) ~ compose(identity, a) ~ a
      * }}}
      *
      * for all `a`, where `~` means "equivalent to".
      */
    type SomeType[A] = zio.Task[A]
    //Option[A]

    def identity: SomeType[Any] = zio.Task.unit
    //Some(())

    val a =
      zio.Task(42)
    //Some(42)

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[(A, B)] =
      left.flatMap(l => right.map(r => (l, r)))

    compose(a, identity).map(_._1) == a
    compose(identity, a).map(_._2) == a

  }

  object Exercise14 {

    /** EXERCISE 14
      *
      * Choose or create a type `SomeType` and a value called `identity` such
      * that you can implement the `compose` function in such a way that:
      *
      * {{{
      * compose(a, identity) ~ compose(identity, a) ~ a
      * }}}
      *
      * for all `a`, where `~` means "equivalent to".
      *
      * Note that `Either[A, Nothing]` is equivalent to `A`, and
      * `Either[Nothing, A]` is equivalent to `A`.
      */
    type SomeType[A] = zio.Task[A] //Option[A], Set[A]

    def identity: SomeType[Nothing] = zio.Task.never //None, Set()

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[Either[A, B]] =
      left.raceEither(right)
    //left.map(Left(_)) orElse right.map(Right(_))
    //left.map(Left(_)) ++ right.map(Right(_))
  }

}

/** IMPERATIVE PATTERNS FOR VALUES - EXERCISE SET 3
  */
object imperative_values {
  trait Exercise1 {

    /** EXERCISE 1
      *
      * Choose or create a data type such that you can implement `andThen` in
      * such a way that it models sequential composition.
      */
    type SomeType = Int => Int

    def andThen(first: SomeType, second: SomeType): SomeType = first andThen second
  }

  trait Exercise2 {

    /** EXERCISE 2
      *
      * Choose or create a different type such that you can implement `andThen` in
      * such a way that it models sequential composition.
      */
    type SomeType

    def andThen(first: SomeType, second: SomeType): SomeType
  }
}

/** IMPERATIVE PATTERNS FOR TYPE CONSTRUCTORS - EXERCISE SET 4
  */
object imperative_tcs {
  trait Exercise1 {

    /** EXERCISE 1
      *
      * Choose or create a data type such that you can implement `andThen` in
      * such a way that it models sequential composition.
      */
    type SomeType[A] = Option[A]

    def andThen[A, B](first: SomeType[A], second: A => SomeType[B]): SomeType[B] =
      first.flatMap(second)
  }

  trait Exercise2 {

    /** EXERCISE 2
      *
      * Choose or create a different type such that you can implement `andThen` in
      * such a way that it models sequential composition.
      */
    type SomeType[A]

    def andThen[A, B](first: SomeType[A], second: A => SomeType[B]): SomeType[B]
  }
}

/** RECIPES - GRADUATION PROJECT
  */
object recipes {

  sealed trait Baked[+A]
  object Baked {
    final case class Burnt[A](value: A)                                     extends Baked[A]
    final case class CookedPerfect[A](value: A)                             extends Baked[A]
    final case class Undercooked[A](value: A)                               extends Baked[A]
    final case class FlatMap[A, B](recipe: Recipe[A], next: A => Recipe[B]) extends Baked[B]
  }

  sealed trait Ingredient
  object Ingredient {
    final case class Eggs(number: Int)        extends Ingredient
    final case class Sugar(amount: Double)    extends Ingredient
    final case class Flour(amount: Double)    extends Ingredient
    final case class Cinnamon(amount: Double) extends Ingredient
  }

  sealed trait Recipe[+A] { self =>

    /** EXERCISE 1
      *
      * Implement a `map` operation that allows changing what a recipe produces.
      */
    def map[B](f: A => B): Recipe[B] = self.flatMap(a => Recipe.succeed(f(a)))

    /** EXERCISE 2
      *
      * Implement a `combine` operation that allows combining two recipes into
      * one, producing both items in a tuple.
      */
    def combine[B](that: Recipe[B]): Recipe[(A, B)] =
      self.flatMap(a => that.map(b => a -> b))

    /** EXERCISE 3
      *
      * Implement a `tryOrElse` operation that allows trying a backup recipe,
      * in case this recipe ends in disaster.
      */
    def tryOrElse[B](that: Recipe[B]): Recipe[Either[A, B]] =
      Recipe.TryOrElse(self.map(Left(_)), that.map(Right(_)))

    def orElse[A1 >: A](that: Recipe[A1]): Recipe[A1] =
      self.tryOrElse(that).map(_.merge)

    /** EXERCISE 4
      *
      * Implement a `flatMap` operation that allows deciding which recipe to
      * make after this recipe has produced its item.
      *
      * NOTE: Be sure to update the `make` method below so that you can make
      * recipes that use your new operation.
      */
    def flatMap[B](f: A => Recipe[B]): Recipe[B] = Recipe.FlatMap(self, f)

    def bake(temp: Int, time: Int): Recipe[Baked[A]] = Recipe.Bake(self, temp, time)
  }

  object Recipe {
    case object Disaster                                                extends Recipe[Nothing]
    final case class Succeed[A](value: A)                               extends Recipe[A]
    final case class AddIngredient(ingredient: Ingredient)              extends Recipe[Ingredient]
    final case class Bake[A](recipe: Recipe[A], temp: Int, time: Int)   extends Recipe[Baked[A]]
    final case class FlatMap[A, B](value: Recipe[A], f: A => Recipe[B]) extends Recipe[B]
    final case class TryOrElse[A](left: Recipe[A], right: Recipe[A])    extends Recipe[A]

    def addIngredient(ingredient: Ingredient): Recipe[Ingredient] = AddIngredient(ingredient)

    def disaster: Recipe[Nothing] = Disaster

    def succeed[A](a: A): Recipe[A] = Succeed(a)
  }

  import Recipe._

  def make[A](recipe: Recipe[A]): A =
    recipe match {
      case Succeed(value)            => value
      case Disaster                  => throw new Exception("Uh no, utter disaster!")
      case AddIngredient(ingredient) => println(s"Adding ${ingredient}"); ingredient
      case Bake(recipe, temp, time) =>
        val a = make(recipe)

        println(s"Baking ${a} for ${time} minutes at ${temp} temperature")

        if (time * temp < 1000) Baked.Undercooked(a)
        else if (time * temp > 6000) Baked.Burnt(a)
        else Baked.CookedPerfect(a)
      case FlatMap(recipe, f) => make(f(make(recipe)))
      case TryOrElse(l, r) =>
        scala.util.Try(make(l)).getOrElse(make(r))
    }

  final case class Cake(ingredients: List[Ingredient])

  /** EXERCISE 5
    *
    * Make a recipe that will produced a baked cake or other food of your choice !
    */
  lazy val recipe: Recipe[Baked[Cake]] =
    (for {
      sugar <- Recipe.addIngredient(Ingredient.Sugar(2.0))
      flour <- Recipe.addIngredient(Ingredient.Flour(4.0))
      spice <- Recipe.addIngredient(Ingredient.Cinnamon(0.01))
      eggs  <- Recipe.addIngredient(Ingredient.Eggs(2))
    } yield Cake(sugar :: flour :: spice :: eggs :: Nil)).bake(350, 20)

}
