package io.circe.validator.internal

import cats.Monad
import cats.instances.string._
import cats.instances.vector._
import cats.mtl.{ApplicativeLocal => AL, FunctorTell => FT}
import cats.syntax.apply._
import cats.syntax.eq._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.traverse._
import io.circe.Json.{False, Null, True}
import io.circe.validator.JsonError.{
  keyNotFound,
  mismatch,
  violation,
  numberCoercion
}
import io.circe.validator.{Env, Errors}
import io.circe.{Json, JsonNumber, JsonObject}
import scala.util.matching.Regex

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
      validators: Vector[F[Unit]]
  ): F[Unit] = {
    val tooFewElements: Int => F[Unit] = l =>
      violation(
        s"Array has less elements ${l} than expected ${validators.length}"
      )

    val sizeCheck: Int => F[Unit] = l =>
      M.whenA(validators.size > l)(tooFewElements(l))

    val arrayValidator0: Vector[Json] => F[Unit] =
      validators
        .zip(_)
        .traverseWithIndexM {
          case ((validator, json), index) =>
            L.local(Env.index(index, json))(validator)
        }
        .void

    withArray(arr => sizeCheck(arr.size) *> arrayValidator0(arr))
  }

  /** @group array */
  def forall(validator: F[Unit]): F[Unit] =
    withArray(
      arr =>
        Vector
          .fill(arr.length)(validator)
          .zip(arr)
          .traverseWithIndexM {
            case ((validator, json), index) =>
              L.local(Env.index(index, json))(validator)
          }
          .void
    )

  // }}} Array -----------------------------------------------------------------
  // {{{ Number ----------------------------------------------------------------

  /** @group number */
  private[internal] def numViolationMsg[A](n: A): String =
    s"Numeric value $n does not satisfy the given predicate."

  /** @group number */
  def eqNumberValidator(expected: JsonNumber): F[Unit] =
    withNumber(
      actual =>
        M.unlessA(actual === expected) {
          lazy val reason =
            s"Number $actual does not equal expected $expected"
          violation(reason)
        }
    )

  /** @group number */
  def withNumber(f: JsonNumber => F[Unit]): F[Unit] =
    L.reader(_.json) >>= (
        json => json.asNumber.fold(mismatch("Number", json))(f)
    )

  /** @group number */
  def number(
      predicate: JsonNumber => Boolean,
      msg: JsonNumber => String = numViolationMsg
  ): F[Unit] = withNumber(liftPredicate(predicate, msg))

  /** @group number */
  def int(
      predicate: Int => Boolean,
      msg: Int => String = numViolationMsg
  ): F[Unit] = {
    def liftOption(optionNum: Option[Int], num: JsonNumber): F[Unit] =
      optionNum.fold(
        numberCoercion("Int", num)
      )(liftPredicate(predicate, msg))

    withNumber(num => liftOption(num.toInt, num))
  }

  /** @group number */
  def bigInt(
      predicate: BigInt => Boolean,
      msg: BigInt => String = numViolationMsg
  ): F[Unit] = {
    def liftOption(optionNum: Option[BigInt], num: JsonNumber): F[Unit] =
      optionNum.fold(
        numberCoercion("BigInt", num)
      )(liftPredicate(predicate, msg))

    withNumber(num => liftOption(num.toBigInt, num))
  }

  /** @group number */
  def bigDecimal(
      predicate: BigDecimal => Boolean,
      msg: BigDecimal => String = numViolationMsg
  ): F[Unit] = {
    def liftOption(optionNum: Option[BigDecimal], num: JsonNumber): F[Unit] =
      optionNum.fold(
        numberCoercion("BigDecimal", num)
      )(liftPredicate(predicate, msg))

    withNumber(num => liftOption(num.toBigDecimal, num))
  }

  /** @group number */
  def long(
      predicate: Long => Boolean,
      msg: Long => String = numViolationMsg
  ): F[Unit] = {
    def liftOption(optionNum: Option[Long], num: JsonNumber): F[Unit] =
      optionNum.fold(
        numberCoercion("Long", num)
      )(liftPredicate(predicate, msg))

    withNumber(num => liftOption(num.toLong, num))
  }

  /** @group number
    *
    * Note: Won't fail on number coercion, but can get truncated in the process.
    */
  def double(
      predicate: Double => Boolean,
      msg: Double => String = numViolationMsg
  ): F[Unit] = {
    withNumber(
      num =>
        liftPredicate(predicate, msg)(
          num.toDouble
        )
    )
  }

  // }}} Number ----------------------------------------------------------------
  // {{{ String ----------------------------------------------------------------

  /** @group string */
  def eqStringValidator(expected: String): F[Unit] =
    withString(
      actual =>
        M.unlessA(actual === expected) {
          val reason = s"String $actual does not match expected $expected"
          violation(reason)
        }
    )

  /** @group string */
  def withString(f: String => F[Unit]): F[Unit] =
    L.reader(_.json) >>= (
        json => json.asString.fold(mismatch("String", json))(f)
    )

  /** @group string */
  def string(
      predicate: String => Boolean,
      msg: String => String = s =>
        s"String value $s does not satisfy the given predicate."
  ): F[Unit] = withString(liftPredicate(predicate, msg))

  /** @group string */
  def regex(regex: Regex): F[Unit] = {
    withString(
      s =>
        M.unlessA(regex.unapplySeq(s).isDefined) {
          val reason =
            s"String value $s does not match regular expression $regex"
          violation(reason)
        }
    )
  }

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
      json => L.local(Env.key(key, json))(validator)
    )

  /** @group object */
  def objectValidator(
      objValidator: Vector[(String, F[Unit])]
  ): F[Unit] = {
    val objectValidator0: JsonObject => F[Unit] = obj =>
      objValidator traverse_ {
        case (key, validator) => atKeyValidator(key, validator)(obj)
      }

    L.reader(_.json) >>= (
        json => json.asObject.fold(mismatch("Object", json))(objectValidator0)
    )
  }

  // }}} Object ----------------------------------------------------------------
  // {{{ Other -----------------------------------------------------------------

  /** group other */
  val pass: F[Unit] = M.unit

  /** group other */
  def failed(msg: String = "Fail unconditionally"): F[Unit] =
    violation(msg)

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
      case otherwise => mismatch(False, otherwise)
    }

  /** group other */
  def nullValidator(): F[Unit] =
    L.reader(_.json) >>= {
      case Null      => M.unit
      case otherwise => mismatch(Null, otherwise)
    }

  // }}} Other -----------------------------------------------------------------
  // {{{ Lifting ---------------------------------------------------------------

  def liftPredicate[A](
      predicate: A => Boolean,
      msg: A => String
  ): A => F[Unit] = a => M.unlessA(predicate(a))(violation(msg(a)))

  // }}} Lifting ---------------------------------------------------------------
}
