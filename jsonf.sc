import $ivy.`io.circe::circe-core:0.11.1`
import $ivy.`org.typelevel::cats-core:1.6.0`
import $ivy.`org.typelevel::cats-mtl-core:0.4.0`
import $plugin.$ivy.`org.spire-math::kind-projector:0.9.3`

import cats.{ Id, Monad, Show }
import cats.instances.string._
import cats.instances.vector._
import cats.syntax.eq._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.show._
import cats.data.{ Chain, ReaderWriterState => RWS, _ }
import cats.mtl.implicits._
import cats.mtl.{ ApplicativeLocal => AL, FunctorTell => FT }
import io.circe.{Json, JsonNumber, JsonObject}
import io.circe.Json._
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

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

type ErrorAt = (Path, JsonError)

type Errors = Chain[ErrorAt]

sealed trait PathStep

object PathStep {

  final case class Key(key: String) extends PathStep
  final case class Index(index: Int) extends PathStep
}

type Path = List[PathStep]

final case class Env(path: Path, json: Json)

object ValidatorF {
	import JsonError._
	import PathStep._

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

type Validator0[A] = RWS[Env, Errors, Unit, A]
type Validator = Validator0[Unit]

object Validator {
  import ValidatorF._

  def trueValidator: Validator = trueValidatorF[Validator0]

  def falseValidator: Validator = falseValidatorF[Validator0]

	def objectValidator(objValidator: Vector[(String, Validator)]): Validator = 
    objectValidatorF[Validator0](objValidator)

	def stringValidator(s: String): Validator = stringValidatorF[Validator0](s)

  def nullValidator: Validator = nullValidatorF[Validator0]

  def numberValidator(num: JsonNumber): Validator = numberValidatorF[Validator0](num)

  def arrayValidator(arrayValidator: Vector[Validator]): Validator = arrayValidatorF[Validator0](arrayValidator)
}

