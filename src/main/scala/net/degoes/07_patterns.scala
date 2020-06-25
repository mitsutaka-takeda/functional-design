package net.degoes

import cats.Id

import scala.util.Try

/*
 * INTRODUCTION
 *
 * In Functional Design, operators that transform and compose values in a
 * domain often fall into pre-existing patterns.
 *
 * In this section, you'll learn to identify these patterns.
 *
 */

/**
 * BINARY COMPOSITION PATTERNS FOR VALUES - EXERCISE SET 1
 */
object binary_values {

  object Exercise1 {

    /**
     * EXERCISE 1
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

    /**
     * EXERCISE 2
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
    type SomeType = String

    def compose(left: SomeType, right: SomeType): SomeType = left + right
  }

  object Exercise3 {

    /**
     * EXERCISE 3
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
    type SomeType = Int

    def compose(left: SomeType, right: SomeType): SomeType = left * right
  }

  object Exercise4 {

    /**
     * EXERCISE 4
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
    type SomeType = Set[Int]

    def compose(left: SomeType, right: SomeType): SomeType = left.union(right)
  }

  object Exercise5 {

    /**
     * EXERCISE 5
     *
     * Choose or create a data type such that your implementation
     * of `compose` represents modeling "both". For example, if you have
     * a data type that represents a query, then this `compose` could
     * combine two queries into one query, such that both results would
     * be queried when the model is executed.
     */
    sealed trait Query

    object Query {

      final case class Both(l: Query, r: Query) extends Query

    }

    def compose(left: Query, right: Query): Query = Query.Both(left, right)
  }

  object Exercise6 {

    /**
     * EXERCISE 6
     *
     * Choose or create a different type such that your implementation
     * of `compose` represents modeling "both".
     */
    sealed trait Filter

    object Filter {

      final case class Both(f1: Filter, f2: Filter) extends Filter

      def compose(left: Filter, right: Filter): Filter = Both(left, right)
    }

  }

  object Exercise7 {

    /**
     * EXERCISE 7
     *
     * Choose or create a data type such that your implementation
     * of `compose` represents modeling "or". For example, if you have
     * a data type that represents a query, then this `compose` could
     * model running one query, but if it fails, running another.
     */
    sealed trait Query

    object Query {

      final case class Or(lhs: Query, rhs: Query) extends Query


      def compose(left: Query, right: Query): Query = Or(left, right)
    }

  }

  object Exercise8 {

    /**
     * EXERCISE 8
     *
     * Choose or create a different type such that your implementation
     * of `compose` represents modeling "or".
     */
    sealed trait Filter

    object Filter {

      final case class Or(lhs: Filter, rhs: Filter) extends Filter

      def compose(left: Filter, right: Filter): Filter = Or(left, right)
    }

  }

  object Exercise9 {

    /**
     * EXERCISE 9
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

    /**
     * EXERCISE 10
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

/**
 * BINARY COMPOSITION PATTERNS FOR TYPE CONSTRUCTORS - EXERCISE SET 2
 */
object binary_tcs {

  object Exercise1 {

    /**
     * EXERCISE 1
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

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[(A, B)] =
      for {
        a <- left
        b <- right
      } yield (a, b)
  }

  object Exercise2 {

    /**
     * EXERCISE 2
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
    type SomeType[A] = Try[A]

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[(A, B)] = for {
      a <- left
      b <- right
    } yield (a, b)
  }

  object Exercise3 {

    /**
     * EXERCISE 3
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
    type SomeType[A]

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[Either[A, B]] = ???
  }

  object Exercise4 {

    /**
     * EXERCISE 4
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
    type SomeType[A] = Option[A]

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[Either[A, B]] = left match {
      case Some(value) => Some(Left(value))
      case None => right.map(Right(_))
    }
  }

  object Exercise5 {

    /**
     * EXERCISE 5
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
    type SomeType[A] = Id[A]

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[(A, B)] = (left, right)
  }

  object Exercise6 {

    /**
     * EXERCISE 6
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
    type SomeType[A] = Option[A]

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[(A, B)] = for {
      x <- left
      y <- right
    } yield (x, y)
  }

  object Exercise7 {

    /**
     * EXERCISE 7
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

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[Either[A, B]] =
      left.map(Left(_): Either[A, B]).union(right.map(Right(_): Either[A, B]))
  }

  object Exercise8 {

    /**
     * EXERCISE 8
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
    type SomeType[A] = List[A]

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[Either[A, B]] =
      left.map(Left(_): Either[A, B]) ++ right.map(Right(_): Either[A, B])
  }

  object Exercise9 {

    /**
     * EXERCISE 9
     *
     * Choose or create a data type such that your implementation
     * of `compose` represents modeling "both". For example, if you have
     * a data type that represents a query, then this `compose` could
     * combine two queries into one query, such that both results would
     * be queried when the model is executed.
     */
    sealed trait Query[A]

    object Query {

      final case class Both[A, B](q1: Query[A], q2: Query[B]) extends Query[(A, B)]

      def compose[A, B](left: Query[A], right: Query[B]): Query[(A, B)] = Both(left, right)
    }

  }

  object Exercise10 {

    /**
     * EXERCISE 10
     *
     * Choose or create a different type such that your implementation
     * of `compose` represents modeling "both".
     */
    sealed trait ComputedValue[A]

    object ComputedValue {

      final case class Both[A, B](left: ComputedValue[A], right: ComputedValue[B]) extends ComputedValue[(A, B)]

      def compose[A, B](left: ComputedValue[A], right: ComputedValue[B]): ComputedValue[(A, B)] = Both(left, right)
    }

  }

  object Exercise11 {

    /**
     * EXERCISE 11
     *
     * Choose or create a data type such that your implementation
     * of `compose` represents modeling "or". For example, if you have
     * a data type that represents a query, then this `compose` could
     * model running one query, but if it fails, running another.
     */
    sealed trait Query[A]

    object Query {

      final case class Or[A, B](first: Query[A], second: Query[B]) extends Query[Either[A, B]]

      def compose[A, B](left: Query[A], right: Query[B]): Query[Either[A, B]] = Or(left, right)
    }

  }

  object Exercise12 {

    /**
     * EXERCISE 12
     *
     * Choose or create a different type such that your implementation
     * of `compose` represents modeling "or".
     */
    sealed trait Filter[A]

    object Filter {

      final case class Or[A, B](first: Filter[A], second: Filter[B]) extends Filter[Either[A, B]]

      def compose[A, B](left: Filter[A], right: Filter[B]): Filter[Either[A, B]] = Or(left, right)
    }

  }

  object Exercise13 {

    /**
     * EXERCISE 13
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
    type SomeType[A] = Option[A]

    def identity: SomeType[Unit] = Some(())

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[(A, B)] = for {
      l <- left
      r <- right
    } yield (l, r)
  }

  object Exercise14 {

    /**
     * EXERCISE 14
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
    type SomeType[A] = Option[A]

    def identity: SomeType[Nothing] = ???

    def compose[A, B](left: SomeType[A], right: SomeType[B]): SomeType[Either[A, B]]
    = left.map(Left(_)).orElse(right.map(Right(_)))
  }

}

/**
 * IMPERATIVE PATTERNS FOR VALUES - EXERCISE SET 3
 */
object imperative_values {

  trait Exercise1 {

    /**
     * EXERCISE 1
     *
     * Choose or create a data type such that you can implement `andThen` in
     * such a way that it models sequential composition.
     *
     * sequential composition ?
     */
    type SomeType = Int

    /**
     * Is this sequential composition?
     */
    def andThen(first: SomeType, second: SomeType): SomeType = first + second
  }

  trait Exercise2 {

    /**
     * EXERCISE 2
     *
     * Choose or create a different type such that you can implement `andThen` in
     * such a way that it models sequential composition.
     */
    type SomeType = Either[String, Int]

    def andThen(first: SomeType, second: SomeType): SomeType = for {
      f <- first
      s <- second
    } yield f + s
  }

}

/**
 * IMPERATIVE PATTERNS FOR TYPE CONSTRUCTORS - EXERCISE SET 4
 */
object imperative_tcs {

  trait Exercise1 {

    /**
     * EXERCISE 1
     *
     * Choose or create a data type such that you can implement `andThen` in
     * such a way that it models sequential composition.
     */
    type SomeType[A] = Either[String, A]

    def andThen[A, B](first: SomeType[A], second: A => SomeType[B]): SomeType[B] = first.flatMap(second)
  }

  trait Exercise2 {

    /**
     * EXERCISE 2
     *
     * Choose or create a different type such that you can implement `andThen` in
     * such a way that it models sequential composition.
     */
    type SomeType[A] = Try[A]

    def andThen[A, B](first: SomeType[A], second: A => SomeType[B]): SomeType[B] = first.flatMap(second)
  }

}

/**
 * RECIPES - GRADUATION PROJECT
 */
object recipes {

  sealed trait Baked[+A]

  object Baked {

    final case class Burnt[A](value: A) extends Baked[A]

    final case class CookedPerfect[A](value: A) extends Baked[A]

    final case class Undercooked[A](value: A) extends Baked[A]

  }

  sealed trait Ingredient

  object Ingredient {

    final case class Eggs(number: Int) extends Ingredient

    final case class Sugar(amount: Double) extends Ingredient

    final case class Flour(amount: Double) extends Ingredient

    final case class Cinnamon(amount: Double) extends Ingredient

  }

  sealed trait Recipe[+A] {
    self =>

    /**
     * EXERCISE 1
     *
     * Implement a `map` operation that allows changing what a recipe produces.
     */
    def map[B](f: A => B): Recipe[B] = Recipe.Map(self, f)

    /**
     * EXERCISE 2
     *
     * Implement a `combine` operation that allows combining two recipes into
     * one, producing both items in a tuple.
     */
    def combine[B](that: Recipe[B]): Recipe[(A, B)] = Recipe.Both(self, that)

    /**
     * EXERCISE 3
     *
     * Implement a `tryOrElse` operation that allows trying a backup recipe,
     * in case this recipe ends in disaster.
     */
    def tryOrElse[B](that: Recipe[B]): Recipe[Either[A, B]] = Recipe.TryOrElse(self, that)

    /**
     * EXERCISE 4
     *
     * Implement a `flatMap` operation that allows deciding which recipe to
     * make after this recipe has produced its item.
     *
     * NOTE: Be sure to update the `make` method below so that you can make
     * recipes that use your new operation.
     */
    def flatMap[B](f: A => Recipe[B]): Recipe[B] = Recipe.FlatMap(self, f)
  }

  object Recipe {

    case object Disaster extends Recipe[Nothing]

    final case class AddIngredient(ingredient: Ingredient) extends Recipe[Ingredient]

    final case class Bake[A](recipe: Recipe[A], temp: Int, time: Int) extends Recipe[Baked[A]]

    final case class Map[A, B](recipe: Recipe[A], f: A => B) extends Recipe[B]

    final case class Both[A, B](r1: Recipe[A], r2: Recipe[B]) extends Recipe[(A, B)]

    final case class TryOrElse[A, B](recipe: Recipe[A], alternative: Recipe[B]) extends Recipe[Either[A, B]]

    final case class FlatMap[A, B](recipe: Recipe[A], f: A => Recipe[B]) extends Recipe[B]

  }

  import Recipe._

  def make[A](recipe: Recipe[A]): A =
    recipe match {
      case Disaster => throw new Exception("Uh no, utter disaster!")
      case AddIngredient(ingredient) => println(s"Adding ${ingredient}"); ingredient
      case Bake(recipe, temp, time) =>
        val a = make(recipe)

        println(s"Baking ${a} for ${time} minutes at ${temp} temperature")

        if (time * temp < 1000) Baked.Undercooked(a)
        else if (time * temp > 6000) Baked.Burnt(a)
        else Baked.CookedPerfect(a)
      case Map(recipe, f) =>
        f(make(recipe))
      case Both(r1, r2) =>
        make(r1) -> make(r2)
      case Recipe.TryOrElse(recipe, alternative) =>
        recipe match {
          case Recipe.Disaster => Right(make(alternative))
          case x => Left(make(x))
        }
      case FlatMap(recipe, f) =>
        make(f(make(recipe)))
    }
}
