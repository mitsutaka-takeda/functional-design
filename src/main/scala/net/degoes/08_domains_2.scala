package net.degoes

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

/**
 * LOYALTY REWARDS PROGRAM - EXERCISE SET 1
 *
 * Consider a software application that manages reward programs for businesses.
 */
object loyalty_program {

  import java.time.Instant

  sealed trait Supplier

  object Supplier {

    case object Amex extends Supplier

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
    // todo I'm not sure if this instance of Numeric for Currency is correct, since I don't take a possibility of different currencies.
    // How can I compare USD to JPY? For example.
    implicit def AmountNumeric[Currency]: Numeric[Amount[Currency]] = new Numeric[Amount[Currency]] {

      override def plus(x: Amount[Currency], y: Amount[Currency]): Amount[Currency] =
        Amount(x.value + y.value, x.currency)

      override def minus(x: Amount[Currency], y: Amount[Currency]): Amount[Currency] =
        Amount(x.value - y.value, x.currency)

      override def times(x: Amount[Currency], y: Amount[Currency]): Amount[Currency] =
        Amount(x.value * y.value, x.currency)

      override def negate(x: Amount[Currency]): Amount[Currency] = Amount(-x.value, x.currency)

      // todo I can't create an instance of Amount[Currency] since here we don't have value for the type Currency.
      override def fromInt(x: Int): Amount[Currency] = ???

      // Amount(BigDecimal(x), FiscalCurrency.USD)

      override def toInt(x: Amount[Currency]): Int = x.value.toInt

      override def toLong(x: Amount[Currency]): Long = x.value.toLong

      override def toFloat(x: Amount[Currency]): Float = x.value.toFloat

      override def toDouble(x: Amount[Currency]): Double = x.value.toDouble

      override def compare(x: Amount[Currency], y: Amount[Currency]): Int = x.value.compare(y.value)
    }
  }

  type FiscalAmount = Amount[FiscalCurrency]
  type LoyaltyAmount = Amount[LoyaltyCurrency]

  final case class Portfolio(holdings: Map[LoyaltyCurrency, LoyaltyAmount]) {
    def +(that: (LoyaltyCurrency, LoyaltyAmount)): Portfolio =
      copy(holdings = holdings.updated(that._1, holdings.get(that._1).map(_ + that._2).getOrElse(that._2)))
  }

  /**
   * Every loyalty program is issued by a supplier, has an associated currency,
   * and has an ordered list of tiers.
   */
  final case class LoyaltyProgram(defaultTier: Tier, tiers: List[Tier], supplier: Supplier, currency: LoyaltyCurrency)

  /**
   * An account is created when a user enrolls in a loyalty program.
   */
  final case class Account(user: User, loyaltyProgram: LoyaltyProgram, tier: Tier, history: List[Activity])

  final case class User(email: String)

  final case class EarnRate(loyaltyCurrency: LoyaltyCurrency, fiscalCurrency: FiscalCurrency)

  /**
   * Tiers contain rule sets that define loyalty point rules for users who are
   * currently in the tier.
   */
  final case class Tier(benefits: Set[Benefit], name: String, description: String, legalese: String, ruleSet: RuleSet)

  final case class Benefit(description: String)

  /*
   * Rule sets represent sets of rules that can perform actions in response
   * to conditions being met. For example, if a user spends so much money,
   * then they may be eligible for an automatic tier promotion.
   */
  sealed trait RuleSet {
    self =>
    /**
     * EXERCISE 1
     *
     * Augment `RuleSet` with an operator that models combining two rule sets
     * into one, applying the actions of both.
     */
    def &&(that: RuleSet): RuleSet = RuleSet.Both(self, that)

    /**
     * EXERCISE 2
     *
     * Augment `RuleSet` with an operator that models combining two rule sets
     * into one, applying either the left (if it results in an action) or the
     * right (if the left does not result in an action)
     */
    def ||(that: RuleSet): RuleSet = RuleSet.OrElse(self, that)
  }

  object RuleSet {

    final case class Both(first: RuleSet, second: RuleSet) extends RuleSet

    final case class OrElse(first: RuleSet, second: RuleSet) extends RuleSet

    /**
     * EXERCISE 3
     *
     * Augment `RuleSet` with a constructor that models execution of a
     * `SystemAction` whenever a `RuleCalculation[Boolean]` evaluates to
     * true.
     */
    final case class When(rule: RuleCalculation[Boolean], action: SystemAction) extends RuleSet

  }

  sealed trait RuleCalculation[+A] {
    self =>

    /**
     * EXERCISE 4
     *
     * Add an operator `&&` that applies only with this calculation and the other
     * calculation produce booleans, and which models the boolean conjunction
     * ("and") of the two boolean values.
     */
    def &&(that: RuleCalculation[Boolean])(implicit ev: A <:< Boolean): RuleCalculation[Boolean] = {
      // This line of code "proves" that the "A" type is actually a Boolean:
      val self1: RuleCalculation[Boolean] = self.widen[Boolean]
      RuleCalculation.And(self1, that)
    }

    /**
     * EXERCISE 5
     *
     * Add an operator `||` that applies only with this calculation and the other
     * calculation produce booleans, and which models the boolean disjunction
     * ("or") of the two boolean values.
     */
    def ||(that: RuleCalculation[Boolean])(implicit ev: A <:< Boolean): RuleCalculation[Boolean] = {
      // This line of code "proves" that the "A" type is actually a Boolean:
      val self1: RuleCalculation[Boolean] = self.widen[Boolean]

      RuleCalculation.Or(self1, that)
    }

    /**
     * EXERCISE 6
     *
     * Add an operator `negate` that applies only with this calculation produces
     * a boolean, and which models the boolean negation of this value.
     */
    def negate(implicit ev: A <:< Boolean): RuleCalculation[Boolean] = {
      // This line of code "proves" that the "A" type is actually a Boolean:
      val self1: RuleCalculation[Boolean] = self.widen[Boolean]

      RuleCalculation.Negate(self1)
    }

    /**
     * EXERCISE 7
     *
     * Add an operator `>` that applies only when this calculation and the
     * other calculation produce amounts, and which models the `>` comparison
     * between the two amounts, which yields a boolean indicating if the
     * relation holds.
     */
    def >[Currency: Numeric](that: RuleCalculation[Currency])(implicit ev: A <:< Currency): RuleCalculation[Boolean] = {
      // This line of code "proves" that the "A" type is actually a Currency:
      val self1: RuleCalculation[Currency] = self.widen[Currency]

      RuleCalculation.GreaterThan(self1, that)
    }

    /**
     * EXERCISE 8
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
      val self1: RuleCalculation[Currency] = self.widen[Currency]
      RuleCalculation.GreaterThanOrEqual(self1, that)
    }

    /**
     * EXERCISE 9
     *
     * Add an operator `<` that applies only when this calculation and the
     * other calculation produce amounts, and which models the `<` comparison
     * between the two amounts, which yields a boolean indicating if the
     * relation holds.
     */
    def <[Currency: Numeric](that: RuleCalculation[Currency])(implicit ev: A <:< Currency): RuleCalculation[Boolean] = {
      // This line of code "proves" that the "A" type is actually a Currency:
      val self1: RuleCalculation[Currency] = self.widen[Currency]
      RuleCalculation.Negate(RuleCalculation.GreaterThanOrEqual(self1, that))
    }

    /**
     * EXERCISE 10
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
      val self1: RuleCalculation[Currency] = self.widen[Currency]
      RuleCalculation.Negate(RuleCalculation.GreaterThan(self1, that))
    }

    def widen[B](implicit ev: A <:< B): RuleCalculation[B] = RuleCalculation.Widen(self)(ev)
  }

  object RuleCalculation {

    final case class Widen[A, B](rc: RuleCalculation[A])(implicit val ev: A <:< B) extends RuleCalculation[B]

    final case class And(lhs: RuleCalculation[Boolean], rhs: RuleCalculation[Boolean]) extends RuleCalculation[Boolean]

    final case class Or(lhs: RuleCalculation[Boolean], rhs: RuleCalculation[Boolean]) extends RuleCalculation[Boolean]

    final case class Negate(rc: RuleCalculation[Boolean]) extends RuleCalculation[Boolean]

    final case class GreaterThan[Currency: Numeric](lhs: RuleCalculation[Currency], rhs: RuleCalculation[Currency]) extends RuleCalculation[Boolean]

    final case class GreaterThanOrEqual[Currency: Numeric](lhs: RuleCalculation[Currency], rhs: RuleCalculation[Currency]) extends RuleCalculation[Boolean]

    /**
     * EXERCISE 11
     *
     * Add a constructor that models calculation of a constant value of the
     * specified type.
     *
     * Is is already solved? Do I need to add function to construct Constant[A]?
     */
    final case class Constant[A](value: A) extends RuleCalculation[A]

    /**
     * EXERCISE 12
     *
     * Add a constructor that models calculation of the price of an item that
     * the user buys, in a fiscal currency.
     *
     * todo Is there any difference between PurchasePrice and ItemPrice?
     */
    final case class PurchasePrice() extends RuleCalculation[FiscalAmount]

    /**
     * EXERCISE 13
     *
     * Add a constructor that models calculation of the price of an item that
     * the user buys, in a fiscal currency.
     *
     * todo What's the difference between PurchasePrise and ItemPrice?
     */
    final case class ItemPrice() extends RuleCalculation[FiscalAmount]

    /**
     * EXERCISE 14
     *
     * Add a constructor that models the number of days since the last purchase
     * of the user, as an integer.
     *
     */
    final case class DaysSinceLastPurchase() extends RuleCalculation[Int]

  }

  sealed trait UserAction

  object UserAction {

    final case class Spend(amount: FiscalCurrency) extends UserAction

    final case class Return(amount: FiscalCurrency) extends UserAction

  }

  sealed trait SystemAction

  object SystemAction {

    final case class Credit(amount: LoyaltyCurrency) extends SystemAction

    final case class Debit(amount: LoyaltyCurrency) extends SystemAction

    case object TierPromotion extends SystemAction

    case object TierDemotion extends SystemAction

    final case class ChangeStatus(status: UserProgramStatus) extends SystemAction

  }

  final case class Activity(action: Either[UserAction, SystemAction], instant: Instant)

  sealed trait UserProgramStatus

  object UserProgramStatus {

    case object Active extends UserProgramStatus

    case object Inactive extends UserProgramStatus

    final case class Suspended(who: User, why: String) extends UserProgramStatus

  }

  /**
   * EXERCISE 15
   *
   * Construct a rule set that describes promotion to the next tier, as
   * well as demotion, and changing the status of the user to inactive.
   */
  def ruleSet: RuleSet = {
    import RuleSet._
    import RuleCalculation._
    import Amount.AmountNumeric

    def dollar(v: BigDecimal) = Constant(Amount(v, FiscalCurrency.USD: FiscalCurrency))

    (When(PurchasePrice() > dollar(200), SystemAction.TierPromotion)
      || (
      When(DaysSinceLastPurchase() > Constant(20), SystemAction.ChangeStatus(UserProgramStatus.Inactive))
        &&
        When(PurchasePrice() < dollar(50), SystemAction.TierDemotion)
      ))
  }

  /**
   * Example of running a rule set on the history of a user to produce system actions.
   *
   * todo How can I implement this?
   */
  def run(history: List[UserAction], ruleSet: RuleSet): List[SystemAction] = ???

  /**
   * Example of describing a rule set in a human-readable form.
   */
  def describe(ruleSet: RuleSet): String = ruleSet match {
    case RuleSet.Both(first, second) => s"apply both ${describe(first)} and ${describe(second)}"
    case RuleSet.OrElse(first, second) => s"apply either ${describe(first)} or ${describe(second)}."
    case RuleSet.When(rule, action) =>
      def describeRule(rule: RuleCalculation[Boolean]): String = ???
      def describeSystemAction(a: SystemAction): String = a match {
        case SystemAction.Credit(amount) => s"credit a user by ${amount.toString}"
        case SystemAction.Debit(amount) => s"debit a user by ${amount.toString}"
        case SystemAction.TierPromotion => "promote a user to next tier"
        case SystemAction.TierDemotion =>"demote a user to lower tier"
        case SystemAction.ChangeStatus(status) => s"change the status of a user to ${status.toString}"
      }

      s"When ${describeRule(rule)}, apply an action: ${describeSystemAction(action)}."
  }
}

/**
 * CALENDER SCHEDULING APP - EXERCISE SET 2
 */
object calendar {

  final case class HourOfDay(value: Int) {
    def to(that: HourOfDay): Stream[HourOfDay] = (value to that.value).toStream.map(HourOfDay(_))

    def until(that: HourOfDay): Stream[HourOfDay] = (value until that.value).toStream.map(HourOfDay(_))
  }

  object HourOfDay {
    val min = HourOfDay(0)
    val max = HourOfDay(24)
  }

  sealed trait DayOfWeek

  object DayOfWeek {

    case object Sunday extends DayOfWeek

    case object Monday extends DayOfWeek

    case object Tuesday extends DayOfWeek

    case object Wednesday extends DayOfWeek

    case object Thursday extends DayOfWeek

    case object Friday extends DayOfWeek

    case object Saturday extends DayOfWeek

  }

  final case class TimeSpan(start: HourOfDay, end: HourOfDay)

  object TimeSpan {
    val empty: TimeSpan = TimeSpan(HourOfDay(0), HourOfDay(0))
  }

  /**
   * EXERCISE 1
   *
   * Explore the structure of `CalendarRegion` by deciding what composable,
   * orthogonal operations to add to the data type.
   */
  final case class CalendarAppointment(span: TimeSpan) {
    self =>
  }

  object CalendarAppointment {
    val empty: CalendarAppointment = CalendarAppointment(TimeSpan.empty)
  }

  /**
   * EXERCISE 2
   *
   * Explore the structure of `DailySchedule` by deciding what composable,
   * orthogonal operations to add to the data type.
   *
   * HINT: Consider the union, intersection, & complement of two daily schedules.
   */
  final case class DailySchedule(set: Set[CalendarAppointment]) {
    self =>
  }

  object DailySchedule {
    val empty: DailySchedule = DailySchedule(Set())
  }

  /**
   * EXERCISE 3
   *
   * Explore the structure of `MonthlySchedule` by deciding what composable,
   * orthogonal operations to add to the data type.
   */
  final case class MonthlySchedule(daysOfMonth: Vector[DailySchedule]) {}

  object MonthlySchedule {
    val empty: MonthlySchedule = MonthlySchedule(Vector())
  }

  final case class Person(name: String)

  /**
   * EXERCISE 4
   *
   * Using the operators you build, express a solution to the following
   * problem: find all the free times that a group of friends can virtually
   * meet for the specified number of hours.
   */
  def findFreeTimes(lengthInHours: Int, friends: Map[Person, MonthlySchedule]): MonthlySchedule = ???

}

/**
 * CMS - EXERCISE SET 3
 *
 * Consider a content-management system.
 */
object cms {

  /**
   * EXERCISE 1
   *
   * Add whatever transformations and combinations have well-defined semantics.
   */
  sealed trait Html {
    self =>
    final def isEmpty: Boolean = self match {
      case Html.Zero => true
      case Html.One(_, _, children) => false
      case Html.Many(elements) => elements.forall(_.isEmpty)
    }

    final def childList: List[Html] = self match {
      case Html.Zero => Nil
      case Html.One(_, _, children) => children.childList
      case Html.Many(elements) => elements.flatMap(_.childList)
    }
  }

  object Html {

    case object Zero extends Html

    final case class One(tagName: String, attributes: Map[String, String], children: Html) extends Html

    final case class Many(elements: List[Html]) extends Html

  }

  final case class User(email: String)

  trait PageContext {
    def url: java.net.URL
  }

  trait UserContext {
    def user: User
  }

  /**
   * EXERCISE 2
   *
   * Add whatever transformations and combinations have well-defined semantics.
   *
   */
  sealed trait Component[-Context, -State] {
    def render(context: Context, state: State): Html
  }

  object Component {

    final case class Leaf[Context, State](render0: (Context, State) => Html) extends Component[Context, State] {
      def render(context: Context, state: State): Html = render0(context, state)
    }

    def make[Context, State](render: (Context, State) => Html): Component[Context, State] = Leaf(render)

    /**
     * EXERCISE 3
     *
     * Add some example components.
     */
    val components = ???
  }

}

/**
 * JSON VALIDATION - EXERCISE SET 4
 *
 * Consider a domain where incoming JSON documents are stored into a NoSQL
 * database, but before being stored, they must be validated by flexible
 * rules. If validation fails, descriptive error messages must be generated
 * that allow clients of the JSON endpoint to fix the issues with their data.
 */
object input_validation {

  sealed trait Json {

    /**
     * EXERCISE 1
     *
     * Implement a method to retrieve the JSON value at the specified path, or
     * fail with a descriptive error message.
     */
    def get(path: JsonPath): Either[String, Json] = ???
  }

  object Json {

    case object Null extends Json

    final case class Bool(value: Boolean) extends Json

    final case class Number(value: BigDecimal) extends Json

    final case class Text(value: String) extends Json

    final case class Sequence(value: List[Json]) extends Json

    final case class Object(value: Map[String, Json]) extends Json

  }

  sealed trait JsonPath {
    self =>
    def +(that: JsonPath): JsonPath =
      that match {
        case JsonPath.Identity => self
        case JsonPath.Field(parent, name) => JsonPath.Field(self + parent, name)
        case JsonPath.Index(parent, index) => JsonPath.Index(self + parent, index)
      }

    def field(name: String): JsonPath = JsonPath.Field(self, name)

    def index(index: Int): JsonPath = JsonPath.Index(self, index)
  }

  object JsonPath {

    case object Identity extends JsonPath

    final case class Field(parent: JsonPath, name: String) extends JsonPath

    final case class Index(parent: JsonPath, index: Int) extends JsonPath

    def identity: JsonPath = Identity
  }

  /**
   * REQUIREMENTS
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

  /**
   * Implement the `validate` function that can validate some JSON and either
   * return descriptive error messages, or succeed with a unit value.
   */
  def validate[A](json: Json, validation: Validation[A]): Either[List[String], A] = ???
}

/**
 * GRADUATION PROJECT
 *
 * Consider a domain where data must be loaded from various sources, processed
 * through a flexible graph of components.
 */
object data_processing {

  sealed trait Schema {
    self =>
    def &(that: Schema): Schema = Schema.Intersect(self, that)

    def |(that: Schema): Schema = Schema.Union(self, that)

    def ??(description: String): Schema = Schema.Described(description, self)
  }

  object Schema {

    case object Null extends Schema

    case object Bool extends Schema

    case object Number extends Schema

    case object Str extends Schema

    case object Date extends Schema

    case object DateTime extends Schema

    final case class Described(description: String, schema: Schema) extends Schema

    final case class Field(key: String, value: Schema) extends Schema

    final case class Intersect(left: Schema, right: Schema) extends Schema

    final case class Union(left: Schema, right: Schema) extends Schema

    final case class Sequence(elementSchema: Schema) extends Schema

    def field(name: String, value: Schema): Schema = Field(name, value)

    lazy val personSchema =
      field("name", Str) &
        field("age", Number) &
        field("address", Str)
    field("manager", personSchema)
  }

  /**
   * Design a data type that can model schema transformations, as well as value
   * transformations (e.g. replacing nulls with values, replacing one type of
   * value with another type.
   */
  sealed trait Transformation

}
