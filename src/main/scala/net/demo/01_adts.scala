package net.demo

import java.time.Instant

/*
 * INTRODUCTION
 *
 * Functional Design depends heavily on functional data modeling. Functional
 * data modeling is the task of creating precise, type-safe models of a given
 * domain using algebraic data types and generalized algebraic data types.
 *
 * In this section, you'll review basic functional domain modeling.
 */

/** A person is someone who has a;
  *  name (String)
  *  age (Int)
  *
  * Product composition
  *
  * A job title is either:
  *  software eng
  *  marketer
  *
  * Sum composition
  */
/** E-COMMERCE - EXERCISE SET 1
  *
  * Consider an e-commerce application that allows users to purchase products.
  */
object credit_card {

  /** EXERCISE 1
    *
    * Using only sealed traits and case classes, create an immutable data model
    * of a credit card, which must have:
    *
    *  * Number
    *  * Name
    *  * Expiration date
    *  * Security code
    */
  //type CreditCard

  sealed abstract case class CreditCardNumber private (value: Long)

  //smart constructor
  object CreditCardNumber {
    def apply(value: Long): Option[CreditCardNumber] =
      if (String.valueOf(value).length == 16) Some(new CreditCardNumber(value) {})
      else None
  }

  /*
  case class CreditCardName private (value: String) extends AnyVal
  object CreditCardName {
    def make(value: String): Option[CreditCardName] = {
      val normalizedValue = value.trim.toUpperCase
      if (normalizedValue.matches("[A-Z ]{2,26}")) Some(CreditCardName(normalizedValue)) else None
    }
  }

  case class ExpirationDate private (month: Int, year: Int)
  object ExpirationDate {
    def make(month: Int, year: Int): Option[ExpirationDate] =
      if (month >= 1 && month <= 12 && year >= 2020) Some(ExpirationDate(month, year)) else None
  }

  case class SecurityCode private (value: Int) extends AnyVal
  object SecurityCode {
    def make(value: Int): Option[SecurityCode] = {
      val length = String.valueOf(value).length
      if (length >= 3 && length <= 4) Some(SecurityCode(value)) else None
     }
  }

  case class CreditCard(
    number: CreditCardNumber,
    name: CreditCardName,
    expirationDate: ExpirationDate,
    securityCode: SecurityCode,
  )*/

  /*sealed abstract case class CCNumber(number: BigInt)
  object CCNumber {

    def apply(number: BigInt): Option[CCNumber] = {
      // https://en.wikipedia.org/wiki/Payment_card_number
      // todo: most cards have 12-19 digits
      // todo: check checksum if applicable for CC type
      def isValid: Boolean = true
      if (isValid) Some(new CCNumber(number) {}) else None
    }
  }

  type CardholderName = String // todo: check constraints, probably limit length, check invalid chars
  type CVV = Int // todo: check constraints, 3-4 digits
  type ExpirationDate = java.time.LocalDate // todo: narrow it down since we don't need days

  case class CreditCard(
    number: CCNumber,
    name: CardholderName,
    expirationDate: ExpirationDate,
    securityCode: CVV,
  )*/

  /*
    sealed trait Digit
    object Digit {
      case object _0 extends Digit
      case object _1 extends Digit
      case object _2 extends Digit
      case object _3 extends Digit
      case object _4 extends Digit
      case object _5 extends Digit
      case object _6 extends Digit
      case object _7 extends Digit
      case object _8 extends Digit
      case object _9 extends Digit
    }
    type Number = List[Digit]
    type Name = String
    type Year = (Digit, Digit)
    type Month = (Digit, Digit)
    case class ExpirationDate(yeah: Year, month: Month)
    case class SecurityCode(first: Digit, second: Digit, third: Digit)
    case class CreditCard(number: Number, name: Name, expirationDate: ExpirationDate, securityCode: SecurityCode)
   */

  /** EXERCISE 2
    *
    * Using only sealed traits and case classes, create an immutable data model
    * of a product, which could be a physical product, such as a gallon of milk,
    * or a digital product, such as a book or movie, or access to an event, such
    * as a music concert or film showing.
    */

  //1st solution
  sealed trait MyProduct {
    def name: String
    def price: BigDecimal
  }

  object MyProduct {
    final case class Physical(name: String, amount: Long, price: BigDecimal) extends MyProduct
    final case class Digital(name: String, url: String, price: BigDecimal)   extends MyProduct
    final case class Event(name: String, price: BigDecimal)                  extends MyProduct
  }

  //downsides: too much boilerplate
  def updatePrice(p: MyProduct, newPrice: BigDecimal) =
    p match {
      case p @ MyProduct.Physical(_, _, _) => p.copy(price = newPrice)
      case p @ MyProduct.Digital(_, _, _)  => p.copy(price = newPrice)
      case p @ MyProduct.Event(_, _)       => p.copy(price = newPrice)
    }

  //2nd solution(less boilerplate)
  sealed trait MyProductType
  object MyProductType {
    object Physical extends MyProductType
    object Digital  extends MyProductType
    object Event    extends MyProductType
  }

  final case class MyProduct1(name: String, amount: Long, price: BigDecimal, pType: MyProductType)

  def updatePrice1(p: MyProduct1, updatedPrice: BigDecimal) =
    p.copy(price = updatedPrice)

  /** EXERCISE 3
    *
    * Using only sealed traits and case classes, create an immutable data model
    * of a product price, which could be one-time purchase fee, or a recurring
    * fee on some regular interval.
    */
  type PricingScheme
}

/** EVENT PROCESSING - EXERCISE SET 3
  *
  * Consider an event processing application, which processes events from both
  * devices, as well as users.
  */
object events {
  /*

  /**
   * EXERCISE
   *
   * Refactor the object-oriented data model in this section to a more
   * functional one, which uses only sealed traits and case classes.
   */

  //Object oriented structure

  abstract class Event(val id: Int) {
    def time: Instant
  }

  // Events are either UserEvent (produced by a user) or DeviceEvent (produced by a device),
  // please don't extend both it will break code!!!
  trait UserEvent extends Event {
    def userName: String
  }

  // Events are either UserEvent (produced by a user) or DeviceEvent (produced by a device),
  // please don't extend both it will break code!!!
  trait DeviceEvent extends Event {
    def deviceId: Int
  }

  class SensorUpdated(id: Int, val deviceId: Int, val time: Instant, val reading: Option[Double])
      extends Event(id)
      with DeviceEvent

  class DeviceActivated(id: Int, val deviceId: Int, val time: Instant) extends Event(id) with DeviceEvent

  class UserPurchase(id: Int, val item: String, val price: Double, val time: Instant, val userName: String)
      extends Event(id)
      with UserEvent

  class UserAccountCreated(id: Int, val userName: String, val time: Instant) extends Event(id) with UserEvent
   */

  //Pull out the common parts and push the differences deeper
  final case class Event(id: Long, when: Instant, content: EventContent)

  //push the sums deeper
  sealed trait EventContent
  object EventContent {
    final case class UserEvent(userId: Long, userEventContent: UserEventContent)         extends EventContent
    final case class DeviceEvent(deviceId: Long, deviceEventContent: DeviceEventContent) extends EventContent
  }

  sealed trait UserEventContent
  object UserEventContent {
    final case class UserPurchase(purchaseId: Int, item: String, price: Double, time: Instant, userName: String)
        extends UserEventContent
    final case class UserAccountCreated(userName: String) extends UserEventContent
  }

  sealed trait DeviceEventContent
  object DeviceEventType {
    final case class SensorUpdated(reading: Option[Double]) extends DeviceEventContent
    final case object DeviceActivated                       extends DeviceEventContent
  }

}

object events2 {

  final case class Event[+Payload](id: Int, time: Instant, eventType: Payload)

  final case class User(userName: String, userDetails: UserDetails)

  sealed trait UserDetails
  object UserDetails {
    final case class Purchase(item: String, price: Double) extends UserDetails
    final case object AccountCreated                       extends UserDetails
  }

  final case class Device(deviceId: Int, deviceDetails: DeviceDetails)

  sealed trait DeviceDetails
  object DeviceDetails {
    final case class Sensor(reading: Option[Double]) extends DeviceDetails
    final case object Activated                      extends DeviceDetails
  }

  def add(event: Event[Device]): Event[Device] =
    event.eventType.deviceDetails match {
      case e: DeviceDetails.Sensor => ???
      case DeviceDetails.Activated => ???
    }

  def add0(event: Event[User]): Event[User] =
    event.eventType.userDetails match {
      case e: UserDetails.Purchase    => ???
      case UserDetails.AccountCreated => ???
    }
}

object events0 {

  sealed trait Event {
    def id: String
    def name: String
  }

  final case class OrderDetails(id: String, name: String)

  final case class PurchasedEvent(id: String, name: String, details: OrderDetails) extends Event
  final case class RefundEvent(id: String, name: String, orderId: Long)            extends Event

  def changeName /*[T <: Event]*/ (e: Event, newName: String): Event = e match {
    case e: PurchasedEvent => e.copy(name = newName) //.asInstanceOf[T]
    case e: RefundEvent    => e.copy(name = newName) //.asInstanceOf[T]
  }

  //OR

  //Encapsulate all the differences into a sum type
  sealed trait Payload
  object Payload {
    final case class Purchase(details: OrderDetails) extends Payload
    final case class Refund(orderId: Long)           extends Payload
  }

  //Commonalities
  final case class Event0(
    id: String,
    name: String,    //common fields
    payload: Payload //differences
  )

  def changeName0(e: Event0, newName: String): Event0 =
    e.copy(name = newName)

}

/** DOCUMENT EDITING - EXERCISE SET 4
  *
  * Consider a web application that allows users to edit and store documents
  * of some type (which is not relevant for these exercises).
  */
object documents {
  final case class UserId(identifier: String)
  final case class DocId(identifier: String)
  final case class DocContent(body: String)

  /** EXERCISE 1
    *
    * Using only sealed traits and case classes, create a simplified but somewhat
    * realistic model of a Document.
    */
  //type Document

  case class Document(id: DocId, user: UserId, cnt: DocContent)

  /** EXERCISE 2
    *
    * Using only sealed traits and case classes, create a model of the access
    * type that a given user might have with respect to a document. For example,
    * some users might have read-only permission on a document.
    */
  //type AccessType
  case class AccessType(verbs: Set[Verb])
  sealed trait Verb
  object Verb {
    case object Read  extends Verb
    case object Write extends Verb
    case object Exec  extends Verb
  }

  case class AccessType2(read: Boolean, write: Boolean, exec: Boolean)

  /** EXERCISE 3
    *
    * Using only sealed traits and case classes, create a model of the
    * permissions that a user has on a set of documents they have access to.
    * Do not store the document contents themselves in this model.
    */
  type DocPermissions
}

/** BANKING - EXERCISE SET 5
  *
  * Consider a banking application that allows users to hold and transfer money.
  */
object bank {

  /** EXERCISE 1
    *
    * Using only sealed traits and case classes, develop a model of a customer at a bank.
    */
  type Customer

  /** EXERCISE 2
    *
    * Using only sealed traits and case classes, develop a model of an account
    * type. For example, one account type allows the user to write checks
    * against a given currency. Another account type allows the user to earn
    * interest at a given rate for the holdings in a given currency.
    */
  type AccountType

  /** EXERCISE 3
    *
    * Using only sealed traits and case classes, develop a model of a bank
    * account, including details on the type of bank account, holdings, customer
    * who owns the bank account, and customers who have access to the bank account.
    */
  type Account
}

/** STOCK PORTFOLIO - GRADUATION PROJECT
  *
  * Consider a web application that allows users to manage their portfolio of investments.
  */
object portfolio {

  /** EXERCISE 1
    *
    * Using only sealed traits and case classes, develop a model of a stock
    * exchange. Ensure there exist values for NASDAQ and NYSE.
    */
  type Exchange

  /** EXERCISE 2
    *
    * Using only sealed traits and case classes, develop a model of a currency
    * type.
    */
  type CurrencyType

  /** EXERCISE 3
    *
    * Using only sealed traits and case classes, develop a model of a stock
    * symbol. Ensure there exists a value for Apple's stock (APPL).
    */
  type StockSymbol

  /** EXERCISE 4
    *
    * Using only sealed traits and case classes, develop a model of a portfolio
    * held by a user of the web application.
    */
  type Portfolio

  /** EXERCISE 5
    *
    * Using only sealed traits and case classes, develop a model of a user of
    * the web application.
    */
  type User

  /** EXERCISE 6
    *
    * Using only sealed traits and case classes, develop a model of a trade type.
    * Example trade types might include Buy and Sell.
    */
  type TradeType

  /** EXERCISE 7
    *
    * Using only sealed traits and case classes, develop a model of a trade,
    * which involves a particular trade type of a specific stock symbol at
    * specific prices.
    */
  type Trade
}
