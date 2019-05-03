package io.circe.validator

import cats.kernel.Eq
import cats.{Monad, Show}
// import cats.syntax.flatMap._
import cats.data.Chain
import cats.implicits._
import cats.mtl.{ApplicativeLocal => AL, FunctorTell => FT}
import io.circe.JsonObject

final case class ErrorAt(at: Path, error: JsonError)

object ErrorAt {
  implicit val eqErrorAt: Eq[ErrorAt] = Eq.fromUniversalEquals
}

/** Sum of all possible validation errors */
sealed trait JsonError

object JsonError extends Instances {

  final case class TypeMismatch(expected: String, got: String) extends JsonError
  final case class PredicateViolation(reason: String)          extends JsonError
  final case class ArrayPredicateViolation[A, B](message: String)
      extends JsonError
  final case class KeyNotFound(key: String, obj: JsonObject) extends JsonError

  def errorAt[F[_]](
      e: JsonError
  )(implicit FT: FT[F, Errors], L: AL[F, Env], M: Monad[F]): F[Unit] =
    L.reader(_.path) >>= (path => FT.tell(Chain.one(ErrorAt(path, e))))

  def mismatch0(a: String, b: String): JsonError = TypeMismatch(a, b)

  def mismatch[F[_], A: Show, B: Show](
      a: A,
      b: B
  )(implicit FT: FT[F, Errors], L: AL[F, Env], M: Monad[F]): F[Unit] =
    errorAt(mismatch0(a.show, b.show))

  val predicateViolation0: String => JsonError = PredicateViolation.apply

  def predicateViolation[F[_]](
      reason: String
  )(implicit FT: FT[F, Errors], L: AL[F, Env], M: Monad[F]): F[Unit] =
    errorAt(predicateViolation0(reason))

  def keyNotFound0(key: String, obj: JsonObject): JsonError =
    KeyNotFound(key, obj)

  def keyNotFound[F[_]](
      a: String,
      b: JsonObject
  )(implicit FT: FT[F, Errors], L: AL[F, Env], M: Monad[F]): F[Unit] =
    errorAt(keyNotFound0(a, b))
}

trait Instances {
  implicit val eqJsonError = Eq.fromUniversalEquals
}
