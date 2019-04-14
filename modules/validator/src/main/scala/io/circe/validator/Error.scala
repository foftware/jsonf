package io.circe.validator

import cats.{ Monad, Show }
import cats.syntax.flatMap._
import cats.syntax.show._
import cats.data.Chain
import cats.mtl.{ ApplicativeLocal => AL, FunctorTell => FT }
import io.circe.JsonObject

sealed trait JsonError

object JsonError {

  final case class TypeMismatch[A, B](expected: A, got: B) extends JsonError
	final case class PredicateViolation[A, B](expected: A, got: B) extends JsonError
	final case class KeyNotFound(key: String, obj: JsonObject) extends JsonError

	def errorAt[F[_]](e: JsonError)(implicit MC: FT[F, Errors], L: AL[F, Env], M: Monad[F]): F[Unit] =
		L.reader(_.path) >>= (path => FT.tell(Chain.one((path, e))))

  def mismatch0[A, B](a: A, b: B): JsonError = TypeMismatch(a, b)

	def mismatch[F[_], A, B](a: A, b: B)(implicit FT: FT[F, Errors], L: AL[F, Env], M: Monad[F]): F[Unit] =
		errorAt(mismatch0(a, b))

	def predicateViolation0[A: Show, B: Show](a: A, b: B): JsonError = PredicateViolation(a.show, b.show)

	def predicateViolation[F[_], A: Show, B: Show](a: A, b: B)(implicit FT: FT[F, Errors], L: AL[F, Env], M: Monad[F]): F[Unit] =
		errorAt(predicateViolation0(a, b))

	def keyNotFound0(key: String, obj: JsonObject): JsonError = KeyNotFound(key, obj)

	def keyNotFound[F[_]](a: String, b: JsonObject)(implicit FT: FT[F, Errors], L: AL[F, Env], M: Monad[F]): F[Unit] =
		errorAt(keyNotFound0(a, b))
}
