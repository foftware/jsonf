// Copyright (c) 2019 Marek Kidoň and František Kocun
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package jsont

import cats.kernel.Eq
import cats.{Monad, Show}
// import cats.syntax.flatMap._
import cats.data.{NonEmptyChain => Nec}
import cats.implicits._
import cats.mtl.{ApplicativeLocal => AL, MonadChronicle => MC}
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
  )(implicit MC: MC[F, Errors], L: AL[F, Env], M: Monad[F]): F[Unit] =
    L.reader(_.path) >>= (path => MC.dictate(Nec.one(ErrorAt(path, e))))

  def mismatch[F[_], A: Show, B: Show](
      a: A,
      b: B
  )(implicit MC: MC[F, Errors], L: AL[F, Env], M: Monad[F]): F[Unit] =
    errorAt(TypeMismatch(a.show, b.show))

  def violation[F[_]](
      reason: String
  )(implicit MC: MC[F, Errors], L: AL[F, Env], M: Monad[F]): F[Unit] =
    errorAt(Violation(reason))

  def keyNotFound[F[_]](
      key: String,
      obj: JsonObject
  )(implicit MC: MC[F, Errors], L: AL[F, Env], M: Monad[F]): F[Unit] =
    errorAt(KeyNotFound(key, obj))

  def numberCoercion[F[_]](
      `type`: String,
      num: JsonNumber
  )(implicit MC: MC[F, Errors], L: AL[F, Env], M: Monad[F]): F[Unit] =
    errorAt(NumberCoercion(`type`, num))
}

trait Instances {
  implicit val eqJsonError = Eq.fromUniversalEquals
}
