package net.degoes

import java.time.Instant

/*
 * INTRODUCTION
 *
 * In Functional Design, a functional domain consists of three things:
 *
 * 1. A set of types that model a solution to a domain problem.
 *
 * 2. Constructors that allow constructing simple solutions.
 *
 * 3. Operators that solving more complex problems by transforming
 *    and combining solutions for subproblems.
 *
 * Functional domains allow modeling solutions to problems in a specific domain.
 * Done properly, a small set of primitives can be so powerful, they can be used
 * compositionally to describe all possible solutions in that domain.
 *
 * A functional domain can be regarded as a type of internal domain-specific
 * language (DSL), which is designed specifically for expressing compositional
 * solutions to some category of domain problems.
 *
 * ZIO is an example of a domain for input/output, whose effect type lets you
 * solve async/concurrent/resourceful problems, and whose operators let you
 * assemble large solutions from small solutions.
 *
 * In this section, you'll learn about designing domains using ADTS,
 * constructors, and composable operators.
 */

/**
 * SPREADSHEET - EXERCISE SET 1
 *
 * Consider a spreadsheet application with a bunch of cells, containing either
 * static data or formula computed from other cells.
 */
object spreadsheet {

  trait Spreadsheet {
    def cols: Int

    def rows: Int

    def valueAt(col: Int, row: Int): CellContents

    final def scan(range: Range): Stream[Cell] = {
      val minRow = range.minRow.getOrElse(0)
      val maxRow = range.maxRow.getOrElse(rows - 1)

      val minCol = range.minCol.getOrElse(0)
      val maxCol = range.maxCol.getOrElse(cols - 1)

      (for {
        col <- (minCol to maxCol).toStream
        row <- (minRow to maxRow).toStream
      } yield Cell(col, row, valueAt(col, row)))
    }
  }

  final case class Range(minRow: Option[Int], maxRow: Option[Int], minCol: Option[Int], maxCol: Option[Int])

  object Range {
    def column(i: Int): Range = Range(None, None, Some(i), Some(i))

    def row(i: Int): Range = Range(Some(i), Some(i), None, None)
  }

  final case class Cell(col: Int, row: Int, contents: CellContents)

  sealed trait CellContents

  object CellContents {

    final case class Error(message: String) extends CellContents

    final case class Str(value: String) extends CellContents

    final case class Dbl(value: Double) extends CellContents

    /**
     * EXERCISE 1
     *
     * Design a subtype of `CellContents` called `CalculatedValue`, which
     * represents a value that is dynamically computed from a spreadsheet.
     */
    final case class CalculatedValue(calculation: Spreadsheet => CellContents) extends CellContents {
      self =>

      /**
       * EXERCISE 2
       *
       * Add some operators to transform one `CalculatedValue` into another `CalculatedValue`. For
       * example, one operator could "negate" a double expression.
       */
      def negate: CalculatedValue = CalculatedValue(
        s =>
          self.calculation(s) match {
            case e: Error => e
            case _: Str => Error("can not negate a string value")
            case Dbl(value) => Dbl(-1 * value)
            case cv: CalculatedValue => cv.negate
          }
      )

      /**
       * EXERCISE 3
       *
       * Add some operators to combine `CalculatedValue`. For example, one operator
       * could sum two double expressions.
       */
      def sum(that: CalculatedValue): CalculatedValue = CalculatedValue(
        s =>
          (self.calculation(s), that.calculation(s)) match {
            case (Error(message), _) => Error(message)
            case (_, Error(message)) => Error(message)
            case (Dbl(v1), Dbl(v2)) => Dbl(v1 + v2)
            case (Str(v1), Str(v2)) => Str(v1 + v2)
            case p@(CalculatedValue(_), CalculatedValue(_)) => p._1.sum(p._2)
          }
      )
    }

    object CalculatedValue {

      /**
       * EXERCISE 4
       *
       * Add a constructor that makes an CalculatedValue from a CellContents.
       */
      def const(contents: CellContents): CalculatedValue = CalculatedValue(_ => contents)

      /**
       * EXERCISE 5
       *
       * Add a constructor that provides access to the value of the
       * specified cell, identified by col/row.
       */
      def at(col: Int, row: Int): CalculatedValue = CalculatedValue(_.valueAt(col, row))
    }

  }

  /**
   * EXERCISE 6
   *
   * Describe a cell whose contents are the sum of other cells.
   */
  lazy val cell1: Cell = Cell(0, 3,
    CellContents.CalculatedValue.at(0, 1).sum(CellContents.CalculatedValue.at(0, 2))
  )
}

/**
 * ETL - EXERCISE SET 2
 *
 * Consider an application designed to extract, transform, and load data.
 */
object etl {

  import scala.util._

  /**
   * Represents a row of data.
   */
  final case class DataRow(row: Map[String, DataValue]) {
    def delete(name: String): DataRow = DataRow(row - name)

    def rename(oldName: String, newName: String): DataRow =
      DataRow(row.get(oldName).fold(row)(value => (row - oldName).updated(newName, value)))
  }

  /**
   * Represents a stream of data.
   */
  final case class DataStream(foreach: (Try[DataRow] => Unit) => Unit) {
    self =>
    def delete(name: String): DataStream = map(_.delete(name))

    def orElse(that: => DataStream): DataStream =
      DataStream { callback =>
        self.foreach {
          case Failure(exception) => that.foreach(callback)
          case x => callback(x)
        }
      }

    def map(f: DataRow => DataRow): DataStream =
      DataStream(callback => self.foreach(a => callback(a.map(f))))

    def rename(oldName: String, newName: String): DataStream =
      self.map(_.rename(oldName, newName))
  }

  /**
   * EXERCISE 1
   *
   * Design a data type that models sources and sinks in an ETL pipeline. Assume
   * your business requires you to extract data from (and load data to) FTP sites,
   * URLs, AWS S3 buckets, and databases described by JDBC connection strings.
   *
   * Also mock out, but do not implement, a method on each repository type called
   * `load`, which returns a `DataStream`.
   */
  sealed trait DataRepo {
    def load: DataStream
  }

  object DataRepo {

    final case class Ftp() extends DataRepo {
      override def load: DataStream = ???
    }

    final case class URL() extends DataRepo {
      override def load: DataStream = ???
    }

    final case class AwsS3() extends DataRepo {
      override def load: DataStream = ???
    }

    final case class Database() extends DataRepo {
      override def load: DataStream = ???
    }

  }

  /**
   * EXERCISE 2
   *
   * Design a data type that models the type of primitives the ETL pipeline
   * has access to. This will include string, numeric, and date/time data.
   */
  sealed trait DataType

  object DataType {

    case object Null extends DataType

    case object Str extends DataType

    case object Numeric extends DataType

    case object DateTime extends DataType

  }

  /**
   * EXERCISE 3
   *
   * Design a data type that models a value. Every value should have a `DataType`
   * that identifies its type (string, numeric, or data/time), and a `coerce` method
   * to coerce the value into another type.
   *
   * Be sure to model null, string, and integer, at the very least!
   */
  sealed trait DataValue {
    def dataType: DataType

    def coerce(otherType: DataType): Option[DataValue]
  }

  object DataValue {

    case object Null extends DataValue {
      self =>
      override def dataType: DataType = DataType.Null

      override def coerce(otherType: DataType): Option[DataValue] = otherType match {
        case DataType.Null => Some(self)
        case DataType.Str => None
        case DataType.Numeric => None
        case DataType.DateTime => None
      }
    }

    final case class Str(value: String) extends DataValue {
      self =>
      override def dataType: DataType = DataType.Str

      override def coerce(otherType: DataType): Option[DataValue] = otherType match {
        case DataType.Null => Some(Null)
        case DataType.Str => Some(self)
        case DataType.Numeric => try {
          Some(Integer(value.toInt))
        } catch {
          case _: NumberFormatException => None
        }
        case DataType.DateTime => try {
          Some(DateTime(Instant.parse(value)))
        } catch {
          case _: NumberFormatException => None
        }
      }
    }

    final case class Integer(value: Long) extends DataValue {
      self =>
      override def dataType: DataType = DataType.Numeric

      override def coerce(otherType: DataType): Option[DataValue] = otherType match {
        case DataType.Null => Some(Null)
        case DataType.Str => Some(Str(value.toString))
        case DataType.Numeric => Some(self)
        case DataType.DateTime => None
      }
    }

    final case class DateTime(value: Instant) extends DataValue {
      self =>
      override def dataType: DataType = DataType.DateTime

      override def coerce(otherType: DataType): Option[DataValue] = otherType match {
        case DataType.Null => Some(Null)
        case DataType.Str => Some(Str(self.value.toString))
        case DataType.Numeric => Some(Integer(self.value.toEpochMilli))
        case DataType.DateTime => Some(self)
      }
    }

  }

  /**
   * `Pipeline` is a data type that models a transformation from an input data
   * set into an output data step, as a series of one or more individual
   * operations.
   */
  final case class Pipeline(run: () => DataStream) {
    self =>

    /**
     * EXERCISE 4
     *
     * Add a `merge` operator that models the merge of the output of this
     * pipeline with the output of the specified pipeline.
     */
    def merge(that: Pipeline): Pipeline = Pipeline(
      () => DataStream(
        callback => {
          self.run().foreach(callback)
          that.run().foreach(callback)
        }
      )
    )

    /**
     * EXERCISE 5
     *
     * Add an `orElse` operator that models applying this pipeline, but if it
     * fails, switching over and trying another pipeline.
     */
    def orElse(that: Pipeline): Pipeline = Pipeline(
      () => self.run().orElse(that.run())
    )

    /**
     * EXERCISE 6
     *
     * Add an operator to rename a column in a pipeline.
     */
    def rename(oldName: String, newName: String): Pipeline = Pipeline(
      () => self.run().rename(oldName, newName)
    )

    /**
     * EXERCISE 7
     *
     * Add an operator to coerce a column into a specific type in a pipeline.
     */
    def coerce(column: String, newType: DataType): Pipeline = Pipeline(
      () => self.run().map {
        old =>
          old.row.get(column).flatMap(_.coerce(newType)) match {
            case Some(value) =>
              old.copy(row = old.row.updated(column, value))
            case None =>
              old
          }
      }
    )

    /**
     * EXERCISE 8
     *
     * Add an operator to delete a column in a pipeline.
     */
    def delete(column: String): Pipeline = Pipeline(
      () => self.run().delete(column)
    )

    /**
     * EXERCISE 9
     *
     * To replace nulls in the specified column with a specified value.
     */
    def replaceNulls(column: String, defaultValue: DataValue): Pipeline = Pipeline(
      () => self.run().map {
        old =>
          old.row.get(column).map {
            case DataValue.Null => defaultValue
            case s: DataValue.Str => s
            case i: DataValue.Integer => i
            case d: DataValue.DateTime => d
          } match {
            case Some(value) =>
              old.copy(row = old.row.updated(column, value))
            case None =>
              old
          }
      }
    )
  }

  object Pipeline {

    /**
     * EXERCISE 10
     *
     * Add a constructor for `Pipeline` that models extraction of data from
     * the specified data repository.
     */
    def extract(repo: DataRepo): Pipeline = Pipeline(
      () => repo.load
    )
  }

  /**
   * EXERCISE 11
   *
   * Create a pipeline that models extracts data from a URL, replacing all null
   * "age" columns with "0" as the default age, which renames a column "fname"
   * into a column "first_name", and which coerces the "age" column into an
   * integer type.
   */
  lazy val pipeline: Pipeline = Pipeline.extract(DataRepo.URL()).replaceNulls("age", DataValue.Str("0")).rename(
    "fname", "first_name"
  ).coerce("age", DataType.Numeric)
}

/**
 * REAL ESTATE APP - GRADUATION PROJECT
 *
 * Consider a real estate app that must regularly fetch third-party pricing data
 * according to specified schedules. These schedules can be quite complicated,
 * although they possess regular structure (e.g. every fifth Tuesday, and hourly
 * on Wednesdays). The business considers it acceptable to create the schedules in
 * code (rather than reading them from a database).
 */
object pricing_fetcher {
  def fetch(directory: java.io.File, url: java.net.URL, schedule: Schedule): Unit = ???

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

  final case class Time(minuteOfHour: Int, hourOfDay: Int, dayOfWeek: DayOfWeek, weekOfMonth: Int, monthOfYear: Int)

  /**
   * `Schedule` is a data type that models a schedule as a simple function,
   * which specifies whether or not it is time to perform a fetch.
   */
  final case class Schedule(fetchNow: Time => Boolean) {
    self =>
    /*
     * EXERCISE 1
     *
     * Create an operator for schedule that allows composing two schedules to
     * yield the union of those schedules. That is, the fetch will occur
     * only when either of the schedules would have performed a fetch.
     */
    def union(that: Schedule): Schedule = Schedule(
      t => self.fetchNow(t) || that.fetchNow(t)
    )

    /**
     * EXERCISE 2
     *
     * Create an operator for schedule that allows composing two schedules to
     * yield the intersection of those schedules. That is, the fetch will occur
     * only when both of the schedules would have performed a fetch.
     */
    def intersection(that: Schedule): Schedule = Schedule(
      t => self.fetchNow(t) && that.fetchNow(t)
    )

    /**
     * EXERCISE 3
     *
     * Create a unary operator that returns a schedule that will never fetch
     * when the original schedule would fetch, and will always fetch when the
     * original schedule would not fetch.
     */
    def negate: Schedule = Schedule(
      t => !self.fetchNow(t)
    )
  }

  object Schedule {

    /**
     * EXERCISE 4
     *
     * Create a constructor for Schedule that models fetching on specific weeks
     * of the month.
     */
    def weeks(weeks: Int*): Schedule = Schedule(
      t =>
        weeks.contains(t.weekOfMonth)
    )

    /**
     * EXERCISE 5
     *
     * Create a constructor for Schedule that models fetching on specific days
     * of the week.
     */
    def daysOfTheWeek(daysOfTheWeek: DayOfWeek*): Schedule = Schedule(
      t =>
        daysOfTheWeek.contains(t.dayOfWeek)
    )

    /**
     * EXERCISE 6
     *
     * Create a constructor for Schedule that models fetching on specific
     * hours of the day.
     */
    def hoursOfTheDay(hours: Int*): Schedule = Schedule(
      t => hours.contains(t.hourOfDay)
    )

    /**
     * EXERCISE 7
     *
     * Create a constructor for Schedule that models fetching on specific minutes
     * of the hour.
     */
    def minutesOfTheHour(minutes: Int*): Schedule = Schedule(
      t => minutes.contains(t.minuteOfHour)
    )
  }

  /**
   * EXERCISE 8
   *
   * Create a schedule that repeats every Wednesday, at 6:00 AM and 12:00 PM,
   * and at 5:30, 6:30, and 7:30 every Thursday.
   */
  lazy val schedule: Schedule = Schedule.daysOfTheWeek(DayOfWeek.Wednesday).intersection(
    Schedule.hoursOfTheDay(6, 12)
  ).union(
    Schedule.daysOfTheWeek(DayOfWeek.Thursday).intersection(
      Schedule.hoursOfTheDay(5, 6, 7).intersection(Schedule.minutesOfTheHour(30))
    )
  )
}
