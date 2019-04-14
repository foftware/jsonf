package io.circe.validator.internal

import cats.Monad
import cats.instances.string._
import cats.instances.vector._
import cats.syntax.eq._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.mtl.{ ApplicativeLocal => AL, FunctorTell => FT }
import io.circe.{ Json, JsonNumber, JsonObject }
import io.circe.Json.{ False, Null, True }

import io.circe.validator.{ Env, Errors }
import io.circe.validator.JsonError.{ mismatch, keyNotFound, predicateViolation }
import io.circe.validator.PathStep.{ Index, Key }

object Validator {
  def trueValidatorF[F[_]](implicit FT: FT[F, Errors], L: AL[F, Env], M: Monad[F]): F[Unit] =
    L.reader(_.json) >>= {
      case True => M.unit
      case otherwise => mismatch(True, otherwise)
    }

  def falseValidatorF[F[_]](implicit FT: FT[F, Errors], L: AL[F, Env], M: Monad[F]): F[Unit] =
    L.reader(_.json) >>= {
      case False => M.unit
      case otherwise => mismatch(True, otherwise)
    }

	def nullValidatorF[F[_]](implicit FT: FT[F, Errors], L: AL[F, Env], M: Monad[F]): F[Unit] =
		L.reader(_.json) >>= {
			case Null => M.unit
			case otherwise => mismatch(Null, otherwise)
		}

	def stringValidatorF[F[_]](s: String)(implicit FT: FT[F, Errors], L: AL[F, Env], M: Monad[F]): F[Unit] =
		stringValidator0( s0 =>
			M.whenA(s =!= s0)(predicateViolation(s, s0))
		)

	def stringValidator0[F[_]](f: String => F[Unit])(implicit FT: FT[F, Errors], L: AL[F, Env], M: Monad[F]): F[Unit] =
		L.reader(_.json) >>= ( json =>
			json.asString.fold(mismatch("String", json))(f)
		)

  def numberValidatorF[F[_]](num: JsonNumber)(implicit FT: FT[F, Errors], L: AL[F, Env], M: Monad[F]): F[Unit] =
    numberValidator0( num0 => M.whenA(num =!= num0)(predicateViolation(num.toString, num0.toString)))

	def numberValidator0[F[_]](f: JsonNumber => F[Unit])(implicit FT: FT[F, Errors], L: AL[F, Env], M: Monad[F]): F[Unit] =
		L.reader(_.json) >>= ( json =>
			json.asNumber.fold(mismatch("Number", json))(f)
		)

	def atKeyValidator[F[_]](
		key: String,
		validator: F[Unit]
	)(
		obj: JsonObject
	)(
		implicit
		FT: FT[F, Errors],
		L: AL[F, Env],
		M: Monad[F]
	): F[Unit] =
		obj(key).fold(
			keyNotFound(key, obj)
		)(
			json => L.local { case Env(path, _) => Env(path :+ Key(key), json) }(validator)
		)

	def objectValidatorF[F[_]](
		objValidator: Vector[(String, F[Unit])]
	)(
		implicit
		FT: FT[F, Errors],
		L: AL[F, Env],
		M: Monad[F]
	): F[Unit] = {
		val objectValidator0: JsonObject => F[Unit] = obj => objValidator.traverse_{
			case (key, validator) => atKeyValidator(key, validator)(obj)
		}

		L.reader(_.json) >>= (json => json.asObject.fold(mismatch("Object", json))(objectValidator0))
	}

  def arrayValidatorF[F[_]](
    arrayValidator: Vector[F[Unit]]
  )(
		implicit
		FT: FT[F, Errors],
		L: AL[F, Env],
		M: Monad[F]
	): F[Unit] = {
    val arrayValidator0: Vector[Json] => F[Unit] = arrayValidator.zipWithIndex.zip(_).traverse_{
      case ((validator, index), json) => L.local { case Env(path, _) => Env(path :+ Index(index), json) }(validator)
    }

    L.reader(_.json) >>= ( json => json.asArray.fold(mismatch("Array", json))(arrayValidator0) )
  }
}
