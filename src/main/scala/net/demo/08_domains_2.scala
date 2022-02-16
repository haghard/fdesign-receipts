package net.demo

/*
 * INTRODUCTION
 *
 * In Functional Design, the most powerful approaches utilize GADTs to model
 * solutions to the domain problem, exposing a composable, expressive, and
 * orthogonal set of primitive constructors and operators, upon which they
 * add derived constructors and operators to bridge gaps between the foundation
 * and common problems in the domain.
 *
 * In this section, you'll tie together everything you've learned to develop
 * hands-on skills in applied functional design.w
 */

/** LOYALTY REWARDS PROGRAM - EXERCISE SET 1
  *
  * Consider a software application that manages reward programs for businesses.
  */

//Pick declarative encoding (model) if you want persist the model(rules) and
//
object loyalty_program {
  import java.time.Instant

  sealed trait Supplier
  object Supplier {
    case object Amex           extends Supplier
    case object UnitedAirlines extends Supplier
  }

  sealed trait LoyaltyCurrency
  object LoyaltyCurrency {
    case object Amex extends LoyaltyCurrency
    type Amex = Amex.type
  }

  sealed trait FiscalCurrency
  object FiscalCurrency {
    final case object USD extends FiscalCurrency
  }

  final case class Amount[Currency](value: BigDecimal, currency: Currency) {
    def +(that: Amount[Currency]): Amount[Currency] =
      copy(value = value + that.value)
  }

  object Amount {
    implicit def AmountNumeric[Currency]: Numeric[Amount[Currency]] = ???
  }

  type FiscalAmount  = Amount[FiscalCurrency]
  type LoyaltyAmount = Amount[LoyaltyCurrency]

  final case class Portfolio(holdings: Map[LoyaltyCurrency, LoyaltyAmount]) {
    def +(that: (LoyaltyCurrency, LoyaltyAmount)): Portfolio =
      copy(holdings = holdings.updated(that._1, holdings.get(that._1).map(_ + that._2).getOrElse(that._2)))
  }

  /** Every loyalty program is issued by a supplier, has an associated currency,
    * and has an ordered list of tiers.
    */
  final case class LoyaltyProgram(defaultTier: Tier, tiers: List[Tier], supplier: Supplier, currency: LoyaltyCurrency)

  /** An account is created when a user enrolls in a loyalty program.
    */
  final case class Account(user: User, loyaltyProgram: LoyaltyProgram, tier: Tier, history: List[Activity])

  final case class User(email: String)

  final case class EarnRate(loyaltyCurrency: LoyaltyCurrency, fiscalCurrency: FiscalCurrency)

  /** Tiers contain rule sets that define loyalty point rules for users who are
    * currently in the tier.
    */
  final case class Tier(benefits: Set[Benefit], name: String, description: String, legalese: String, ruleSet: RuleSet)

  final case class Benefit(description: String)

  /*
   * Rule sets represent sets of rules that can perform actions in response
   * to conditions being met. For example, if a user spends so much money,
   * then they may be eligible for an automatic tier promotion.
   */
  //declarative enc
  sealed trait RuleSet { self =>

    /** EXERCISE 1
      *
      * Augment `RuleSet` with an operator that models combining two rule sets
      * into one, applying either the left (if it results in an action) or the
      * right (if the left does not result in an action)
      */
    def &&(that: RuleSet): RuleSet = RuleSet.Both(self, that)

    def ||(that: RuleSet): RuleSet = RuleSet.OrElse(self, that)
  }

  object RuleSet {

    final case class Both(a: RuleSet, b: RuleSet)   extends RuleSet
    final case class OrElse(a: RuleSet, b: RuleSet) extends RuleSet

    /** EXERCISE 2
      *
      * Augment `RuleSet` with a constructor that models execution of a
      * `SystemAction` whenever a `RuleCalculation[Boolean]` evaluates to
      * true.
      */
    final case class When(calc: RuleCalculation[Boolean], action: SystemAction) extends RuleSet

    def when(cond: RuleCalculation[Boolean], action: SystemAction) = When(cond, action)
  }

  sealed trait RuleCalculation[+A] { self =>

    /** EXERCISE 3
      *
      * Add an operator `&&` that applies only with this calculation and the other
      * calculation produce booleans, and which models the boolean conjunction
      * ("and") of the two boolean values.
      */
    def &&(that: RuleCalculation[Boolean])(implicit ev: A <:< Boolean): RuleCalculation[Boolean] = {
      // This line of code "proves" that the "A" type is actually a Boolean:
      val self1: RuleCalculation[Boolean] = self.as[Boolean]
      RuleCalculation.Both(self1, that)
    }

    /** EXERCISE 4
      *
      * Add an operator `||` that applies only with this calculation and the other
      * calculation produce booleans, and which models the boolean disjunction
      * ("or") of the two boolean values.
      */
    def ||(that: RuleCalculation[Boolean])(implicit ev: A <:< Boolean): RuleCalculation[Boolean] = {
      // This line of code "proves" that the "A" type is actually a Boolean:
      val self1: RuleCalculation[Boolean] = self.as[Boolean]
      !(!self1 && !that)
    }

    /** EXERCISE 5
      *
      * Add an operator `negate` that applies only with this calculation produces
      * a boolean, and which models the boolean negation of this value.
      */
    def unary_!(implicit ev: A <:< Boolean): RuleCalculation[Boolean] = {
      // This line of code "proves" that the "A" type is actually a Boolean:
      val self1: RuleCalculation[Boolean] = self.as[Boolean]
      RuleCalculation.Not(self1)
    }

    /** EXERCISE 6
      *
      * Add an operator `>` that applies only when this calculation and the
      * other calculation produce amounts, and which models the `>` comparison
      * between the two amounts, which yields a boolean indicating if the
      * relation holds.
      */
    def >[Currency: Numeric](that: RuleCalculation[Currency])(implicit ev: A <:< Currency): RuleCalculation[Boolean] = {
      // This line of code "proves" that the "A" type is actually a Currency:
      val self1: RuleCalculation[Currency] = self.as[Currency]
      that < self1
    }

    /** EXERCISE 7
      *
      * Add an operator `>=` that applies only when this calculation and the
      * other calculation produce amounts, and which models the `>=` comparison
      * between the two amounts, which yields a boolean indicating if the
      * relation holds.
      */
    def >=[Currency: Numeric](
      that: RuleCalculation[Currency]
    )(implicit ev: A <:< Currency): RuleCalculation[Boolean] = {
      // This line of code "proves" that the "A" type is actually a Currency:
      val self1: RuleCalculation[Currency] = self.as[Currency]
      that <= self1
    }

    /** EXERCISE 8
      *
      * Add an operator `<` that applies only when this calculation and the
      * other calculation produce amounts, and which models the `<` comparison
      * between the two amounts, which yields a boolean indicating if the
      * relation holds.
      */
    def <[Currency: Numeric](that: RuleCalculation[Currency])(implicit ev: A <:< Currency): RuleCalculation[Boolean] = {
      val self1: RuleCalculation[Currency] = self.as[Currency]
      (self1 <= that) && !(self1 === that)
    }

    /** EXERCISE 9
      *
      * Add an operator `<=` that applies only when this calculation and the
      * other calculation produce amounts, and which models the `<=` comparison
      * between the two amounts, which yields a boolean indicating if the
      * relation holds.
      */
    def <=[Currency: Numeric](
      that: RuleCalculation[Currency]
    )(implicit ev: A <:< Currency): RuleCalculation[Boolean] = {
      // This line of code "proves" that the "A" type is actually a Currency:
      val self1: RuleCalculation[Currency] = self.as[Currency]
      //that <= self1
      RuleCalculation.LessOrEqualTo(self1, that)
    }

    /** EXERCISE 10
      *
      * Add an operator `===` that applies only when this calculation and the other calculation
      * produce amounts, and which models equality.
      */
    def ===[Currency: Numeric](
      that: RuleCalculation[Currency]
    )(implicit ev: A <:< Currency): RuleCalculation[Boolean] = {
      val self1: RuleCalculation[Currency] = self.as[Currency]
      (self1 <= that) && (that <= self1)
    }

    def as[B](implicit ev: A <:< B): RuleCalculation[B] = RuleCalculation.Widen(self)
  }

  object RuleCalculation {
    final case class Widen[A, B](rc: RuleCalculation[A])(implicit val ev: A <:< B) extends RuleCalculation[B]

    final case class Not(self: RuleCalculation[Boolean]) extends RuleCalculation[Boolean]
    final case class LessOrEqualTo[A: Numeric](a: RuleCalculation[A], b: RuleCalculation[A])
        extends RuleCalculation[Boolean]

    final case class Both(a: RuleCalculation[Boolean], b: RuleCalculation[Boolean]) extends RuleCalculation[Boolean]

    /** EXERCISE 10
      *
      * Add a constructor that models calculation of a constant value of the
      * specified type.
      */
    final case class Constant[A](value: A) extends RuleCalculation[A]

    final case class PurchaseAmount[A](value: Int) extends RuleCalculation[FiscalAmount]

    /** EXERCISE 11
      *
      * Add a constructor that models calculation of the price of an item that
      * the user buys, in a fiscal currency.
      */
    final case object PurchasePrice extends RuleCalculation[FiscalAmount]

    /** EXERCISE 12
      *
      * Add a constructor that models calculation of the price of an item that
      * the user buys, in a fiscal currency.
      */
    final case object ItemPrice extends RuleCalculation[FiscalAmount]

    /** EXERCISE 13
      *
      * Add a constructor that models the number of days since the last purchase
      * of the user, as an integer.
      */
    final case object DaysSinceLastPurchase extends RuleCalculation[Int]
  }

  sealed trait UserAction
  object UserAction {
    final case class Spend(amount: FiscalCurrency)  extends UserAction
    final case class Return(amount: FiscalCurrency) extends UserAction
  }

  sealed trait SystemAction
  object SystemAction {
    final case class Credit(amount: LoyaltyCurrency)         extends SystemAction
    final case class Debit(amount: LoyaltyCurrency)          extends SystemAction
    case object TierPromotion                                extends SystemAction
    case object TierDemotion                                 extends SystemAction
    final case class ChangeStatus(status: UserProgramStatus) extends SystemAction
  }

  final case class Activity(action: Either[UserAction, SystemAction], instant: Instant)

  sealed trait UserProgramStatus
  object UserProgramStatus {
    case object Active                                 extends UserProgramStatus
    case object Inactive                               extends UserProgramStatus
    final case class Suspended(who: User, why: String) extends UserProgramStatus
  }

  import Amount._

  /** EXERCISE 14
    *
    * Construct a rule set that describes promotion to the next tier, as
    * well as demotion, and changing the status of the user to inactive.
    */
  def ruleSet: RuleSet =
    /*
    val a = RuleCalculation.DaysSinceLastPurchase > RuleCalculation.Constant(30)
    val b = RuleCalculation.DaysSinceLastPurchase > RuleCalculation.Constant(365)

    RuleSet
      .when(a, SystemAction.TierDemotion)
      .&&(RuleSet.when(b, SystemAction.TierDemotion))
     */

    RuleSet.when(RuleCalculation.DaysSinceLastPurchase > RuleCalculation.Constant(30), SystemAction.TierDemotion) ||
    RuleSet.when(RuleCalculation.PurchasePrice.>(RuleCalculation.PurchaseAmount(100)), SystemAction.TierPromotion) &&
    RuleSet.when(
      RuleCalculation.DaysSinceLastPurchase > RuleCalculation.Constant(365),
      SystemAction.ChangeStatus(UserProgramStatus.Inactive)
    )

  /** Example of running a rule set on the history of a user to produce system actions.
    */
  def run(history: List[UserAction], ruleSet: RuleSet): List[SystemAction] = ???

  /** Example of describing a rule set in a human-readable form.
    */
  def describe(ruleSet: RuleSet): String = ???
}

/** CALENDER SCHEDULING APP - EXERCISE SET 2
  */
object calendar {
  final case class HourOfDay(value: Int) {
    def to(that: HourOfDay): Stream[HourOfDay] = (value to that.value).toStream.map(HourOfDay(_))

    def until(that: HourOfDay): Stream[HourOfDay] = (value until that.value).toStream.map(HourOfDay(_))

    def <(that: HourOfDay): Boolean     = ???
    def >(that: HourOfDay): Boolean     = ???
    def max(that: HourOfDay): HourOfDay = ???
    def min(that: HourOfDay): HourOfDay = ???

    def -(that: HourOfDay): HourOfDay = ???

    def *(that: HourOfDay): HourOfDay = ???

  }
  object HourOfDay {
    val min = HourOfDay(0)
    val max = HourOfDay(24)
  }

  sealed trait DayOfWeek
  object DayOfWeek {
    case object Sunday    extends DayOfWeek
    case object Monday    extends DayOfWeek
    case object Tuesday   extends DayOfWeek
    case object Wednesday extends DayOfWeek
    case object Thursday  extends DayOfWeek
    case object Friday    extends DayOfWeek
    case object Saturday  extends DayOfWeek
  }

  final case class TimeSpan(start: HourOfDay, end: HourOfDay) {
    def length: Int = end.value - start.value
  }
  object TimeSpan {
    val empty: TimeSpan = TimeSpan(HourOfDay(0), HourOfDay(0))
  }

  /** EXERCISE 1
    *
    * Explore the structure of `CalendarAppointment` by deciding what composable,
    * orthogonal operations to add to the data type.
    */
  //Executable because it could have been span: () => TimeSpan
  final case class CalendarAppointment(span: TimeSpan) { self =>

    def complement: Set[CalendarAppointment] =
      (if (span.start.value > 0) Set(CalendarAppointment(TimeSpan(HourOfDay(0), span.start)))
       else Set()) ++
      (if (span.end.value < 24) Set(CalendarAppointment(TimeSpan(span.end, HourOfDay(24))))
       else Set())

    def length: Int = span.length

    def *(scalar: Int): CalendarAppointment =
      CalendarAppointment(
        TimeSpan(
          span.start,
          HourOfDay(((span.end.value - span.start.value) * scalar) min 24)
        )
      )

    def intersect(that: CalendarAppointment): CalendarAppointment =
      if (self.overlaps(that)) {
        val newStart = self.span.start max that.span.start
        val newEnd   = self.span.end min that.span.end
        CalendarAppointment(TimeSpan(newStart, newEnd))
      } else CalendarAppointment.empty

    def min(that: CalendarAppointment): CalendarAppointment =
      if (self.span.start.value < that.span.start.value) self else that

    def max(that: CalendarAppointment): CalendarAppointment =
      if (self.span.end.value > that.span.end.value) self else that

    def overlaps(that: CalendarAppointment): Boolean =
      !(self.span.end < that.span.start || that.span.end < self.span.start)

    def union(that: CalendarAppointment): Option[CalendarAppointment] = {
      val newStart = (self min that).span.start
      val newEnd   = (self max that).span.end
      if (overlaps(that)) Some(CalendarAppointment(TimeSpan(newStart, newEnd)))
      else None
    }
  }

  object CalendarAppointment {
    val empty: CalendarAppointment = CalendarAppointment(TimeSpan.empty)
    val full: CalendarAppointment  = CalendarAppointment(TimeSpan(HourOfDay(0), HourOfDay(24)))
    //boundaries
  }

  /** EXERCISE 2
    *
    * Explore the structure of `DailySchedule` by deciding what composable,
    * orthogonal operations to add to the data type.
    */
  /*
  final case class DailySchedule(set: Set[CalendarAppointment]) { self =>

    def + (appt: CalendarAppointment) =
      DailySchedule(if (!set.exists(_.overlaps(appt))) set + appt else set)

    def intersect(that: DailySchedule) =
      DailySchedule(for {
        l <- self.set
        r <- that.set
      } yield l intersect r).merge

    def merge =
      if (set.isEmpty) self else set.reduce((a, b) => a.union(b).fold(a)(identity))
  }*/

  final case class DailySchedule(set: Set[CalendarAppointment]) { self =>

    def busy(appt: CalendarAppointment): Boolean = set.exists(_.overlaps(appt))

    def complement: DailySchedule = DailySchedule(set.flatMap(_.complement)).normalize

    def free(appt: CalendarAppointment): Boolean = !busy(appt)

    def intersect(that: DailySchedule): DailySchedule =
      DailySchedule(for {
        a1 <- self.set
        a2 <- that.set
      } yield a1 intersect a2).normalize

    def normalize: DailySchedule = ???
    /*set.fold(Set.empty[CalendarAppointment]) {
        case (acc, appt1) =>
          acc.fold(Set.empty[CalendarAppointment]) {
            case (acc, appt2) => acc ++ appt2.union(appt2).toSet
          }
      }*/

    def union(that: DailySchedule): DailySchedule =
      DailySchedule(self.set union (that.set)).normalize
  }

  object DailySchedule {
    val empty: DailySchedule = DailySchedule(Set())
  }

  /** EXERCISE 3
    *
    * Explore the structure of `MonthlySchedule` by deciding what composable,
    * orthogonal operations to add to the data type.
    */

  final case class MonthlySchedule(daysOfMonth: Vector[DailySchedule]) { self =>
    def complement: MonthlySchedule = MonthlySchedule(daysOfMonth.map(_.complement))

    def intersect(that: MonthlySchedule): MonthlySchedule =
      MonthlySchedule(self.daysOfMonth.zip(that.daysOfMonth).map { case (l, r) => l intersect r })

    def union(that: MonthlySchedule): MonthlySchedule =
      MonthlySchedule(self.daysOfMonth.zip(that.daysOfMonth).map { case (l, r) => l union r })
  }
  object MonthlySchedule {
    val empty: MonthlySchedule = MonthlySchedule(Vector.fill(31)(DailySchedule.empty))
    val full: MonthlySchedule  = empty.complement
  }

  final case class Person(name: String)

  /** EXERCISE 4
    *
    * Using the operators you build, express a solution to the following
    * problem: find all the free times that a group of friends can virtually
    * meet for the specified number of hours.
    */
  def findFreeTimes(lengthInHours: Int, friends: Map[Person, MonthlySchedule]): MonthlySchedule =
    friends.values.reduceOption(_ union _).getOrElse(MonthlySchedule.empty)

}

/** CMS - EXERCISE SET 3
  *
  * Consider a content-management system.
  */
object cms {

  /** EXERCISE 1
    *
    * Add whatever transformations and combinations have well-defined semantics.
    */
  sealed trait Html { self =>
    final def isEmpty: Boolean =
      self match {
        case Html.Zero                => true
        case Html.One(_, _, children) => false
        case Html.Many(elements)      => elements.forall(_.isEmpty)
      }

    final def childList: List[Html] =
      self match {
        case Html.Zero                => Nil
        case Html.One(_, _, children) => children.childList
        case Html.Many(elements)      => elements.flatMap(_.childList)
      }
  }
  object Html {
    case object Zero                                                                       extends Html
    final case class One(tagName: String, attributes: Map[String, String], children: Html) extends Html
    final case class Many(elements: List[Html])                                            extends Html
  }

  final case class User(email: String)

  trait PageContext {
    def url: java.net.URL
  }
  trait UserContext {
    def user: User
  }

  /** EXERCISE 2
    *
    * Add whatever transformations and combinations have well-defined semantics.
    */
  sealed trait Component[-Context, -State] { // contravariant

    def render(context: Context, state: State): Html

    def embed(header: Html, footer: Html): Component[Context, State] = ???

    //binary composition
    def ++[Context1 <: Context, State2](that: Component[Context1, State2]): Component[Context1, (State, State2)] =
      ???

    def choice[Context1 <: Context, State2](
      that: Component[Context1, State2]
    ): Component[Context1, Either[State, State2]] =
      ???
  }

  object Component {
    final case class Leaf[Context, State](render0: (Context, State) => Html) extends Component[Context, State] {
      def render(context: Context, state: State): Html = render0(context, state)
    }

    def make[Context, State](render: (Context, State) => Html): Component[Context, State] = Leaf(render)

    /** EXERCISE 3
      *
      * Add some example components.
      */
    val components = ???
  }
}

/** JSON VALIDATION - EXERCISE SET 4
  *
  * Consider a domain where incoming JSON documents are stored into a NoSQL
  * database, but before being stored, they must be validated by flexible
  * rules. If validation fails, descriptive error messages must be generated
  * that allow clients of the JSON endpoint to fix the issues with their data.
  */
object input_validation {
  sealed trait Json {

    /** EXERCISE 1
      *
      * Implement a method to retrieve the JSON value at the specified path, or
      * fail with a descriptive error message.
      */
    def get(path: JsonPath): Either[String, Json] = ???
  }
  object Json {
    case object Null                                  extends Json
    final case class Bool(value: Boolean)             extends Json
    final case class Number(value: BigDecimal)        extends Json
    final case class Text(value: String)              extends Json
    final case class Sequence(value: List[Json])      extends Json
    final case class Object(value: Map[String, Json]) extends Json
  }

  sealed trait JsonPath { self =>
    def +(that: JsonPath): JsonPath =
      that match {
        case JsonPath.Identity             => self
        case JsonPath.Field(parent, name)  => JsonPath.Field(self + parent, name)
        case JsonPath.Index(parent, index) => JsonPath.Index(self + parent, index)
      }

    def field(name: String): JsonPath = JsonPath.Field(self, name)

    def index(index: Int): JsonPath = JsonPath.Index(self, index)
  }
  object JsonPath {
    case object Identity                                   extends JsonPath
    final case class Field(parent: JsonPath, name: String) extends JsonPath
    final case class Index(parent: JsonPath, index: Int)   extends JsonPath

    def identity: JsonPath = Identity
  }

  /** REQUIREMENTS
    *
    * 1. Verify that a JSON path is a string, number, null, boolean, object, or array.
    * 2. Verify that numbers are integer, non-zero, or within some range.
    * 3. Verify that one part of a JSON value constraints another part.
    * 4. Verify that a string can be interpreted as an ISO date time.
    * 5. Verify that an object has a field.
    * 6. Verify that an array has a certain minimum length.
    * 7. Verify that a field in an object meets certain requirements.
    * 8. Verify that an element in an array meets certain requirements.
    * 9. Verify that all elements in an array meet certain requirements.
    */
  type Validation[+A]
  object Validation {}

  /** Implement the `validate` function that can validate some JSON and either
    * return descriptive error messages, or succeed with a unit value.
    */
  def validate[A](json: Json, validation: Validation[A]): Either[List[String], A] = ???
}

/** GRADUATION PROJECT
  *
  * Consider a domain where data must be loaded from various sources, processed
  * through a flexible graph of components.
  */
object data_processing {

  sealed trait Schema { self =>
    // Arbitrary & schema == schema & Arbitrary == schema
    def &(that: => Schema): Schema = Schema.Intersect(() => self, () => that)

    // Arbitrary | schema == schema | Arbitrary == schema
    def |(that: => Schema): Schema = Schema.Union(() => self, () => that)

    def ??(description: String): Schema = Schema.Described(description, self)
  }

  object Schema {
    case object Arbitrary                                             extends Schema
    case object Null                                                  extends Schema
    case object Bool                                                  extends Schema
    case object Number                                                extends Schema
    case object Str                                                   extends Schema
    case object Date                                                  extends Schema
    case object DateTime                                              extends Schema
    final case class Described(description: String, schema: Schema)   extends Schema
    final case class HasField(name: String, value: () => Schema)       extends Schema
    final case class Intersect(left: () => Schema, right: () => Schema) extends Schema
    final case class Union(left: () => Schema, right: () => Schema)     extends Schema
    final case class Sequence(elementSchema: () => Schema)             extends Schema

    def hasField(name: String, value: => Schema): Schema = HasField(name, () => value)

    val number: Schema    = Number
    val string: Schema    = Str
    val bool: Schema      = Bool
    val arbitrary: Schema = Arbitrary
    val date: Schema      = Date
    val dateTime          = DateTime
    val nulls: Schema     = Null

    def list(schema: => Schema): Schema = Sequence(() => schema)

    lazy val personSchema: Schema =
      hasField("name", string) &
      hasField("age", number | nulls) &
      hasField("address", list(string)) &
      hasField("manager", personSchema | nulls)
  }

  sealed trait Data {
    def schema: Schema
  }
  object Data {
    abstract class AbstractData(val schema: Schema)

    case object Null                                          extends AbstractData(Schema.Null)
    final case class Bool(value: Boolean)                     extends AbstractData(Schema.Bool)
    final case class Number(value: BigDecimal)                extends AbstractData(Schema.Number)
    final case class Str(value: String)                       extends AbstractData(Schema.Str)
    final case class Date(value: java.time.LocalDate)         extends AbstractData(Schema.Date)
    final case class DateTime(value: java.time.LocalDateTime) extends AbstractData(Schema.DateTime)
    final case class Record(value: Map[String, Data]) extends Data {
      def schema =
        value.map { case (name, data) => Schema.hasField(name, data.schema) }.reduceOption(_ & _) match {
          case Some(value) => value
          case None        => Schema.arbitrary
        }
    }
    final case class Sequence(value: List[Data]) extends Data {
      def schema =
        value.map(_.schema).toSet.reduceOption(_ | _) match {
          case Some(value) => value
          case None        => Schema.arbitrary
        }
    }
  }

  /** EXERCISE 1
    *
    * Design a data type that can model schema transformations, as well as value
    * transformations (e.g. replacing nulls with values, replacing one type of
    * value with another type.
    */
  sealed trait Transformation {}
  object Transformation       {}
}
