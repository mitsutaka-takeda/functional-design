package net.degoes

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

/**
 * E-COMMERCE - EXERCISE SET 1
 *
 * Consider an e-commerce application that allows users to purchase products.
 */
object credit_card {

  /**
   * EXERCISE 1
   *
   * Using only sealed traits and case classes, create an immutable data model
   * of a credit card, which must have:
   *
   * * Number
   * * Name
   * * Expiration date
   * * Security code
   */
  final case class Number(value: Long)

  final case class Name(value: String)

  final case class ExpirationDate(value: Instant)

  final case class SecurityCode(value: String)

  final case class CreditCard(number: Number, name: Name, expirationDate: ExpirationDate, securityCode: SecurityCode)

  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, create an immutable data model
   * of a product, which could be a physical product, such as a gallon of milk,
   * or a digital product, such as a book or movie, or access to an event, such
   * as a music concert or film showing.
   */
  sealed trait Product

  object Product {

    sealed trait PhysicalProduct extends Product

    sealed trait DigitalProduct extends Product

  }

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, create an immutable data model
   * of a product price, which could be one-time purchase fee, or a recurring
   * fee on some regular interval.
   */
  sealed trait PricingScheme

  object PricingScheme {

    sealed trait OneTimePurchaseFee extends PricingScheme

    sealed trait RecurringFee extends PricingScheme

  }

}

/**
 * EVENT PROCESSING - EXERCISE SET 3
 *
 * Consider an event processing application, which processes events from both
 * devices, as well as users.
 */
object events {

  /**
   * EXERCISE
   *
   * Refactor the object-oriented data model in this section to a more
   * functional one, which uses only sealed traits and case classes.
   */
  final case class EventId(id: Int)

  sealed trait Event {
    def id: Int

    def time: Instant
  }

  sealed trait UserEvent extends Event {
    def userName: String
  }

  sealed trait DeviceEvent extends Event {
    def deviceId: Int
  }


  //  abstract class Event(val id: Int) {
  //
  //    def time: Instant
  //  }
  //
  //  // Events are either UserEvent (produced by a user) or DeviceEvent (produced by a device),
  //  // please don't extend both it will break code!!!
  //  trait UserEvent extends Event {
  //    def userName: String
  //  }
  //
  //  // Events are either UserEvent (produced by a user) or DeviceEvent (produced by a device),
  //  // please don't extend both it will break code!!!
  //  trait DeviceEvent extends Event {
  //    def deviceId: Int
  //  }

  case class SensorUpdated(id: Int, deviceId: Int, time: Instant, reading: Option[Double])
    extends DeviceEvent

  case class DeviceActivated(id: Int, deviceId: Int, time: Instant) extends DeviceEvent

  case class UserPurchase(id: Int, item: String, price: Double, time: Instant, userName: String)
    extends UserEvent

  case class UserAccountCreated(id: Int, userName: String, time: Instant) extends UserEvent

}

/**
 * DOCUMENT EDITING - EXERCISE SET 4
 *
 * Consider a web application that allows users to edit and store documents
 * of some type (which is not relevant for these exercises).
 */
object documents {

  final case class UserId(identifier: String)

  final case class DocId(identifier: String)

  final case class DocContent(body: String)

  /**
   * EXERCISE 1
   *
   * Using only sealed traits and case classes, create a simplified but somewhat
   * realistic model of a Document.
   */
  final case class Document(id: DocId, author: UserId, content: DocContent, createdAt: Instant)

  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, create a model of the access
   * type that a given user might have with respect to a document. For example,
   * some users might have read-only permission on a document.
   */
  sealed trait AccessType

  sealed trait ReadOnly extends AccessType
  sealed trait Write extends AccessType

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, create a model of the
   * permissions that a user has on a set of documents they have access to.
   * Do not store the document contents themselves in this model.
   */
  final case class DocPermissions(permissions: Map[DocId, AccessType])
}

/**
 * BANKING - EXERCISE SET 5
 *
 * Consider a banking application that allows users to hold and transfer money.
 */
object bank {

  /**
   * EXERCISE 1
   *
   * Using only sealed traits and case classes, develop a model of a customer at a bank.
   */
  final case class CustomerId(identifier: String)
  final case class CustomerName(name: String)
  final case class Customer(id: CustomerId, name: CustomerName)

  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, develop a model of an account
   * type. For example, one account type allows the user to write checks
   * against a given currency. Another account type allows the user to earn
   * interest at a given rate for the holdings in a given currency.
   */
  sealed trait AccountType
  sealed trait Checking extends AccountType
  sealed trait Saving extends AccountType

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, develop a model of a bank
   * account, including details on the type of bank account, holdings, customer
   * who owns the bank account, and customers who have access to the bank account.
   */
  final case class Account(owner: Customer, accountType: AccountType, customers: Set[CustomerId])
}

/**
 * STOCK PORTFOLIO - GRADUATION PROJECT
 *
 * Consider a web application that allows users to manage their portfolio of investments.
 */
object portfolio {

  /**
   * EXERCISE 1
   *
   * Using only sealed traits and case classes, develop a model of a stock
   * exchange. Ensure there exist values for NASDAQ and NYSE.
   */
  type Exchange

  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, develop a model of a currency
   * type.
   */
  type CurrencyType

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, develop a model of a stock
   * symbol. Ensure there exists a value for Apple's stock (APPL).
   */
  type StockSymbol

  /**
   * EXERCISE 4
   *
   * Using only sealed traits and case classes, develop a model of a portfolio
   * held by a user of the web application.
   */
  type Portfolio

  /**
   * EXERCISE 5
   *
   * Using only sealed traits and case classes, develop a model of a user of
   * the web application.
   */
  type User

  /**
   * EXERCISE 6
   *
   * Using only sealed traits and case classes, develop a model of a trade type.
   * Example trade types might include Buy and Sell.
   */
  type TradeType

  /**
   * EXERCISE 7
   *
   * Using only sealed traits and case classes, develop a model of a trade,
   * which involves a particular trade type of a specific stock symbol at
   * specific prices.
   */
  type Trade
}
