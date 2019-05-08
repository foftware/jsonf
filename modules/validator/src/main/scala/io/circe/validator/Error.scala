package io.circe.validator

import cats.kernel.Eq
import cats.{Monad, Show}
// import cats.syntax.flatMap._
import cats.data.Chain
import cats.implicits._
import cats.mtl.{ApplicativeLocal => AL, FunctorTell => FT}
import io.circe.{JsonObject, JsonNumber}

final case class ErrorAt(at: Path, error: JsonError)

object ErrorAt {
  implicit val eqErrorAt: Eq[ErrorAt] = Eq.fromUniversalEquals
}

/** Sum of all possible validation errors */
sealed trait JsonError

object JsonError extends Instances {

  final case class TypeMismatch(expected: String, got: String) extends JsonError
  final case class Violation(reason: String)                   extends JsonError
  final case class KeyNotFound(key: String, obj: JsonObject)   extends JsonError
  final case class NumberCoercion(`type`: String, num: JsonNumber)
      extends JsonError

  def errorAt[F[_]](
      e: JsonError
  )(implicit FT: FT[F, Errors], L: AL[F, Env], M: Monad[F]): F[Unit] =
    L.reader(_.path) >>= (path => FT.tell(Chain.one(ErrorAt(path, e))))

  def mismatch[F[_], A: Show, B: Show](
      a: A,
      b: B
  )(implicit FT: FT[F, Errors], L: AL[F, Env], M: Monad[F]): F[Unit] =
    errorAt(TypeMismatch(a.show, b.show))

  def violation[F[_]](
      reason: String
  )(implicit FT: FT[F, Errors], L: AL[F, Env], M: Monad[F]): F[Unit] =
    errorAt(Violation(reason))

  def keyNotFound[F[_]](
      key: String,
      obj: JsonObject
  )(implicit FT: FT[F, Errors], L: AL[F, Env], M: Monad[F]): F[Unit] =
    errorAt(KeyNotFound(key, obj))

  def numberCoercion[F[_]](
      `type`: String,
      num: JsonNumber
  )(implicit FT: FT[F, Errors], L: AL[F, Env], M: Monad[F]): F[Unit] =
    errorAt(NumberCoercion(`type`, num))
}

trait Instances {
  implicit val eqJsonError = Eq.fromUniversalEquals
}
