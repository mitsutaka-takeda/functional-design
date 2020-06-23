package net.degoes

/*
 * INTRODUCTION
 *
 * In Functional Design, type safety of data models can be greatly improved by
 * using so-called generalized algebraic data types.
 *
 * In this section, you'll review GADTs with a focus on functional models.
 *
 */

/**
 * EXPRESSIONS - EXERCISE SET 1
 *
 * Consider an application (such as the spreadsheet example) that needs to
 * calculate values in a user-defined way.
 */
object expr {

  sealed trait CalculatedValue[+A]

  object CalculatedValue {

    final case class Integer(value: Int) extends CalculatedValue[Int]

    final case class Str(value: String) extends CalculatedValue[String]

    /**
     * EXERCISE 1
     *
     * Add an operator that adds two integer expressions, yielding an integer
     * expression.
     *
     * NOTE: Be sure to modify the `calculate` method below, so that it can
     * handle the new operation.
     */
    final case class Add(left: CalculatedValue[Int], right: CalculatedValue[Int]) extends CalculatedValue[Int]

    /**
     * EXERCISE 2
     *
     * Add an operator that subtracts an integer from another integer expression,
     * yielding an integer expression.
     *
     * NOTE: Be sure to modify the `calculate` method below, so that it can
     * handle the new operation.
     */
    final case class Subtract(left: CalculatedValue[Int], right: CalculatedValue[Int]) extends CalculatedValue[Int]

    /**
     * EXERCISE 3
     *
     * Add an operator that multiplies two integer expressions, yielding an
     * integer expression.
     *
     * NOTE: Be sure to modify the `calculate` method below, so that it can
     * handle the new operation.
     */
    final case class Multiply(left: CalculatedValue[Int], right: CalculatedValue[Int]) extends CalculatedValue[Int]

    /**
     * EXERCISE 4
     *
     * Add an operator that concatenates two strings, yielding a string
     * expression.
     *
     * NOTE: Be sure to modify the `calculate` method below, so that it can
     * handle the new operation.
     */
    final case class Concat(left: CalculatedValue[String], right: CalculatedValue[String]) extends CalculatedValue[String]

    /**
     * EXERCISE 5
     *
     * Add an operator that determines if a string starts with a specified
     * prefix, yielding a boolean expression.
     *
     * NOTE: Be sure to modify the `calculate` method below, so that it can
     * handle the new operation.
     */
    final case class StartsWith(str: CalculatedValue[String], prefix: String) extends CalculatedValue[Boolean]

  }

  import CalculatedValue._

  def calculate[A](expr: CalculatedValue[A]): A =
    expr match {
      case Integer(v) => v
      case Str(v) => v
      case Add(left, right) =>
        calculate(left) + calculate(right)
      case Subtract(left, right) =>
        calculate(left) - calculate(right)
      case Multiply(left, right) =>
        calculate(left) * calculate(right)
      case Concat(left, right) =>
        calculate(left) + calculate(right)
      case StartsWith(str, prefix) =>
        calculate(str).startsWith(prefix)
    }
}

/**
 * PARSERS - EXERCISE SET 2
 */
object parser {

  sealed trait Parser[+A]

  object Parser {

    final case object OneChar extends Parser[Char]

    /**
     * EXERCISE 1
     *
     * Add an operator that can repeat a parser between some lower range
     * (optional) and some upper range (optional).
     *
     * NOTE: Be sure to modify the `parse` method below, so that it can
     * handle the new operation.
     */
    final case class Repeat[A](min: Option[Int], max: Option[Int], parser: Parser[A]) extends Parser[A]

    /**
     * EXERCISE 2
     *
     * Add a constructor that models the production of the specified value (of
     * any type at all), without consuming any input.
     *
     * NOTE: Be sure to modify the `parse` method below, so that it can
     * handle the new operation.
     */
    final case class Succeed[A](a: A) extends Parser[A]

    /**
     * EXERCISE 3
     *
     * Add a constructor that models failure with a string error message.
     *
     * NOTE: Be sure to modify the `parse` method below, so that it can
     * handle the new operation.
     */
    final case class Fail(error: String) extends Parser[Nothing]

    /**
     * EXERCISE 4
     *
     * Add an operator that can try one parser, but if that fails, try
     * another parser.
     *
     * NOTE: Be sure to modify the `parse` method below, so that it can
     * handle the new operation.
     */
    final case class OrElse[A](first: Parser[A], alternative: Parser[A]) extends Parser[A]

    /**
     * EXERCISE 5
     *
     * Add an operator that parses one thing, and then parses another one,
     * in sequence, producing a tuple of their results.
     *
     * NOTE: Be sure to modify the `parse` method below, so that it can
     * handle the new operation.
     */
    final case class Sequence[A, B](first: Parser[A], second: Parser[B]) extends Parser[(A, B)]

  }

  import Parser._

  def parse[A](parser: Parser[A], input: String): Either[String, (String, A)] =
    parser match {
      case OneChar =>
        input.headOption
          .map((a: Char) => Right(input.drop(1) -> a))
          .getOrElse(Left("The input to the parser has no remaining characters"))
      case Succeed(a) => Right(input -> a)
      case Fail(error) => Left(error)
      case OrElse(first, alternative) =>
        parse(first, input).fold(
          _ => parse(alternative, input),
          i => Right(i)
        )
      case Sequence(first, second) =>
        for {
          r1 <- parse(first, input)
          r2 <- parse(second, r1._1)
        } yield (r2._1, (r1._2, r2._2))
      case Repeat(min, max, parser) =>
        val m = min.getOrElse(0)
        val mx = max.getOrElse(m)
        val baseline = (0 to m).foldLeft(Right(input -> None): Either[String, (String, Option[A])]) {
          case (e, _) =>
            e.flatMap {
              case (i, _) => parse(parser, i).map { case (i, v) => i -> Some(v) }
            }
        }
        val after = (0 to (mx - m)).foldLeft(baseline) {
          case (e, _) =>
            e.flatMap {
              case (i, _) => parse(parser, i).map { case (i, v) => i -> Some(v) }
            }
        }
        after.flatMap { case (i, maybeOutput) => maybeOutput.toRight("no output").map { o => i -> o } }
    }
}
