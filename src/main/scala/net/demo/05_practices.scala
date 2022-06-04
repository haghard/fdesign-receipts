package net.demo

/*
 * INTRODUCTION
 *
 * In Functional Design, composable operators allow building infinitely many
 * solutions from a few operators and domain constructors.
 *
 * Operators and constructors are either primitive, meaning they cannot be
 * expressed in terms of others, or they are derived, meaning they can be
 * expressed in terms of other operators or constructors.
 *
 * The choice of primitives determine how powerful and expressive a domain
 * model is. Some choices lead to weaker models, and others, to more powerful
 * models. Power is not always a good thing: constraining the power of a model
 * allows more efficient and more feature-full execution.
 *
 * Derived operators and constructors bridge the gap from the domain, to common
 * problems that a user of the domain has to solve, improving productivity.
 *
 * In many domains, there exist many potential choices for the set of primitive
 * operators and constructors. But not all choices are equally good.
 *
 * The best primitives are:
 *
 * * Composable, to permit a lot of power in a small, reasonable package
 * * Expressive, to solve the full range of problems in the domain
 * * Orthogonal, such that no primitive provides the capabilities of any other
 *
 * Orthogonality also implies minimalism, which means the primitives are the
 * smallest set of orthogonal primitives that exist.
 *
 */

/** ORTHOGONALITY - EXERCISE SET 1
  */
object email_filter3 {
  final case class Address(emailAddress: String)
  final case class Email(sender: Address, to: List[Address], subject: String, body: String)

  /** EXERCISE 1
    *
    * In the following model, which describes an email filter, there are many primitives with overlapping
    * responsibilities. Find the smallest possible set of primitive operators and constructors, without deleting any
    * constructors or operators (you may implement them in terms of primitives).
    *
    * NOTE: You may *not* use a final encoding, which would allow you to collapse everything down to one primitive.
    */
  sealed trait EmailFilter { self => // declarative

    def &&(that: EmailFilter): EmailFilter = EmailFilter.And(self, that)

    def ||(that: EmailFilter): EmailFilter = !(!self.&&(!that))

    def ^^(that: EmailFilter): EmailFilter = (self || that) && !(self && that)

    def unary_! : EmailFilter = EmailFilter.Negate(self)

  }

  object EmailFilter {
    final case object Always                   extends EmailFilter
    final case class Negate(left: EmailFilter) extends EmailFilter

    final case class And(left: EmailFilter, right: EmailFilter) extends EmailFilter
    final case class SenderEquals(target: Address)              extends EmailFilter
    final case class RecipientIncludes(target: Address)         extends EmailFilter

    final case class BodyContains(phrase: String)    extends EmailFilter
    final case class SubjectContains(phrase: String) extends EmailFilter

    val always: EmailFilter = Always

    val never: EmailFilter = !always

    def senderIs(sender: Address): EmailFilter = SenderEquals(sender)

    def senderIsNot(sender: Address): EmailFilter = !senderIs(sender)

    def recipientIncludes(recipient: Address): EmailFilter = RecipientIncludes(recipient)

    def recipientExcludes(recipient: Address): EmailFilter = !recipientIncludes(recipient)

    def senderIn(senders: Set[Address]): EmailFilter =
      senders.map(senderIs(_)).fold(never)(_ || _)

    def recipientIn(recipients: Set[Address]): EmailFilter =
      recipients.map(recipientIncludes(_)).fold(never)(_ || _)

    def bodyContains(phrase: String): EmailFilter = BodyContains(phrase)

    def bodyDoesNotContain(phrase: String): EmailFilter = !bodyContains(phrase)

    def subjectContains(phrase: String): EmailFilter = SubjectContains(phrase)

    def subjectDoesNotContain(phrase: String): EmailFilter = !subjectContains(phrase)
  }
}

/** COMPOSABILITY - EXERCISE SET 2
  */
object ui_components {

  /** EXERCISE 1
    *
    * The following API is not composable—there is no domain. Introduce a domain with elements, constructors, and
    * composable operators.
    */
  trait Turtle { self =>

    def turnLeft(degrees: Int): Unit

    def turnRight(degrees: Int): Unit

    def goForward(): Unit

    def goBackward(): Unit

    def draw(): Unit

  }

  object executable {

    // or Turtle => Turtle
    final case class Turtle2(run: Turtle => Unit) { self =>
      def +(that: Turtle2): Turtle2 =
        Turtle2 { turtle => self.run(turtle); that.run(turtle) }

      // def goForward: Turtle2               = self + Turtle2.goForward
      // def turnLeft(degrees: Int): Turtle2  = self + Turtle2.turnLeft(degrees)
      // def turnRight(degrees: Int): Turtle2 = self + Turtle2.turnRight(degrees)
      // def draw: Turtle2                    = self + Turtle2.draw
    }

    object Turtle2 {

      val start: Turtle2 = Turtle2(_ => ())

      def turnLeft(degrees: Int): Turtle2 = Turtle2(_.turnLeft(degrees))

      def turnRight(degrees: Int): Turtle2 = turnLeft(360 - degrees) // for orthogonality
      // Turtle2(_.turnRight(degrees))

      val goForward: Turtle2 = Turtle2(_.goForward())

      val goBackward: Turtle2 = turnLeft(180) + goForward + turnLeft(180)
      // Turtle2(_.goBackward())

      val draw: Turtle2 = Turtle2(_.draw())
    }

    import Turtle2._

    (start + turnLeft(100) + turnRight(50) + draw + goForward).run(???)

    // start.turnLeft(100).goForward.draw.run(???)

  }

  object declarative {

    sealed trait Turtle2 { self =>
      def +(that: Turtle2): Turtle2 = Turtle2.Both(self, that)
    }

    object Turtle2 {
      final case object Draw                               extends Turtle2
      final case object GoForward                          extends Turtle2
      final case class Both(left: Turtle2, right: Turtle2) extends Turtle2
      final case class TurnRight(degrees: Int)             extends Turtle2

      val start     = turnLeft(0)
      val draw      = Draw
      val goForward = GoForward

      def goBackward              = TurnRight(180) + goForward + TurnRight(180)
      def turnLeft(degrees: Int)  = TurnRight(360 - degrees)
      def turnRight(degrees: Int) = TurnRight(degrees)
    }

    def run(turtle2: Turtle2, turtle: Turtle): Unit =
      turtle2 match {
        case Turtle2.Both(left, right) =>
          run(left, turtle)
          run(right, turtle)
        case Turtle2.GoForward          => turtle.goForward()
        case Turtle2.TurnRight(degrees) => turtle.turnRight(degrees)
        case Turtle2.Draw               => turtle.draw()
      }

    import Turtle2._

    val drawning = start + goForward + turnLeft(180) + goBackward + draw
    run(drawning, ???)
  }

  object declarative2 {
    sealed trait Turtle3 { self =>
      def +(that: Turtle3): Turtle3 = Turtle3.Both(self, that)

      def goForward: Turtle3               = self + Turtle3.GoForward
      def goBackward: Turtle3              = self + Turtle3.GoForward
      def turnRight(degrees: Int): Turtle3 = self + Turtle3.TurnRight(degrees)
      def turnLeft(degrees: Int): Turtle3  = self + Turtle3.TurnRight(360 - degrees)
      def draw: Turtle3                    = self + Turtle3.Draw
    }

    object Turtle3 {
      final case object Draw                               extends Turtle3
      final case object GoForward                          extends Turtle3
      final case class Both(left: Turtle3, right: Turtle3) extends Turtle3
      final case class TurnRight(degrees: Int)             extends Turtle3

      val start: Turtle3 = TurnRight(0)
    }

    import Turtle3._
    def run(turtle3: Turtle3, turtle: Turtle): Unit =
      turtle3 match {
        case Both(left, right) =>
          run(left, turtle)
          run(right, turtle)
        case GoForward          => turtle.goForward()
        case TurnRight(degrees) => turtle.turnRight(degrees)
        case Draw               => turtle.draw()
      }

    val drawning = start.goBackward.goForward.turnLeft(30).turnRight(50).draw
    run(drawning, ???)
  }
}
