package io.circe.validator.internal

import cats.Monad
import cats.data.EitherT
import cats.instances.string._
import cats.instances.vector._
import cats.mtl.{ApplicativeLocal => AL, FunctorTell => FT}
import cats.syntax.eq._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import io.circe.Json.{False, Null, True}
import io.circe.validator.JsonError.{keyNotFound, mismatch, predicateViolation}
import io.circe.validator.PathStep.{Index, Key}
import io.circe.validator.{Env, Errors}
import io.circe.{Json, JsonNumber, JsonObject}

/** Generic validator
  *
  * @group array
  * @group number
  * @group string
  * @group object
  * @group other
  * @group lifting
  *
  * @groupdesc array "Array validators"
  * @groupdesc number "Number validators"
  * @groupdesc string "String validators"
  * @groupdesc object "Object validators"
  * @groupdesc other "Singleton and other validators"
  * @groupdesc lifting "Utility functions for lifting into 'ValidatorF'"
  */
abstract class ValidatorF[F[_]](
   implicit
   FT: FT[F, Errors],
   L: AL[F, Env],
   M: Monad[F]
) {

  // {{{ Array -----------------------------------------------------------------

  /** @group array */
  def withArray(
      onArray: Vector[Json] => F[Unit]
  ): F[Unit] =
    L.reader(_.json) >>= (
      json => json.asArray.fold(mismatch("Array", json))(onArray)
    )

  /** @group array */
  def arrayValidator(
      arrayValidator: Vector[F[Unit]]
  ): F[Unit] = {
    val arrayValidator0: Vector[Json] => F[Unit] =
      arrayValidator.zipWithIndex.zip(_).traverse_ {
        case ((validator, index), json) =>
          L.local { case Env(path, _) => Env(path :+ Index(index), json) }(
            validator
          )
      }

    withArray(arrayValidator0)
  }

  // }}} Array -----------------------------------------------------------------
  // {{{ Number ----------------------------------------------------------------

  /** @group number */
  def eqNumberValidator(num: JsonNumber): F[Unit] =
    withNumber(num0 => M.whenA(num =!= num0){
      lazy val reason = s"Number: $num does not match expected $num0"
      predicateViolation(reason)
    })

  /** @group number */
  def withNumber(f: JsonNumber => F[Unit]): F[Unit] =
    L.reader(_.json) >>= (
        json => json.asNumber.fold(mismatch("Number", json))(f)
    )

  // }}} Number ----------------------------------------------------------------
  // {{{ String ----------------------------------------------------------------

  /** @group string */
  def eqStringValidator(s: String): F[Unit] =
    withString(s0 => M.whenA(s =!= s0){
      val reason = s"String: $s does not match expected $s0"
      predicateViolation(reason)
    })

  /** @group string */
  def withString(f: String => F[Unit]): F[Unit] =
    L.reader(_.json) >>= (
        json => json.asString.fold(mismatch("String", json))(f)
    )

  // }}} String ----------------------------------------------------------------
  // {{{ Object ----------------------------------------------------------------

  /** @group object */
  private[internal] def atKeyValidator(
      key: String,
      validator: F[Unit]
  )(
      obj: JsonObject
  ): F[Unit] =
    obj(key).fold(
      keyNotFound(key, obj)
    )(
      json =>
        L.local { case Env(path, _) => Env(path :+ Key(key), json) }(validator)
    )

  /** @group object */
  def objectValidator(
      objValidator: Vector[(String, F[Unit])]
  ): F[Unit] = {
    val objectValidator0: JsonObject => F[Unit] = obj =>
      objValidator.traverse_ {
        case (key, validator) => atKeyValidator(key, validator)(obj)
      }

    L.reader(_.json) >>= (
        json => json.asObject.fold(mismatch("Object", json))(objectValidator0)
    )
  }

  // }}} Object ----------------------------------------------------------------
  // {{{ Other -----------------------------------------------------------------

  /** group other */
  def withJson(validator: Json => F[Unit]): F[Unit] =
    L.reader(_.json) >>= validator

  /** group other */
  def trueValidator(): F[Unit] =
    L.reader(_.json) >>= {
      case True      => M.unit
      case otherwise => mismatch(True, otherwise)
    }

  /** group other */
  def falseValidator(): F[Unit] =
    L.reader(_.json) >>= {
      case False     => M.unit
      case otherwise => mismatch(True, otherwise)
    }

  /** group other */
  def nullValidator(): F[Unit] =
    L.reader(_.json) >>= {
      case Null      => M.unit
      case otherwise => mismatch(Null, otherwise)
    }

  // }}} Other -----------------------------------------------------------------
  // {{{ Lifting ---------------------------------------------------------------

  /** group other */
  def liftEither[A](
      validator: A => Either[String, Unit],
  ): A => F[Unit] = a =>
    EitherT.fromEither[F](validator(a)).valueOrF(predicateViolation(_))

  /** group other */
  val liftJsonEither: (Json => Either[String, Unit]) => F[Unit] =
    (withJson _) compose liftEither

  /** group other */
  def liftArrayEither: (Vector[Json] => Either[String, Unit]) => F[Unit] =
    (withArray _) compose liftEither

  /** group other */
  def liftString: (String => Either[String, Unit]) => F[Unit] =
    (withString _) compose liftEither

  /** group other */
  def liftNumber: (JsonNumber => Either[String, Unit]) => F[Unit] =
    (withNumber _) compose liftEither

  // }}} Lifting ---------------------------------------------------------------

  def s0(predicate: sourcecode.Text[String => Boolean]): F[Unit] =
    liftString(s0 => Either.cond(predicate.value(s0), (), predicate.source))

  // Doesn't work though yet
  def s(predicate: String => Boolean): F[Unit] = s0(predicate)
}
