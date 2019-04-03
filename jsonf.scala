import $ivy.`io.circe::circe-core:0.11.1`
import $ivy.`io.circe::circe-literal:0.11.1`
import $ivy.`org.typelevel::cats-core:1.6.0`
import $plugin.$ivy.`org.spire-math::kind-projector:0.9.3`

import cats.{Applicative, Monoid}
import cats.data.{Kleisli, Validated}
import cats.implicits._
import io.circe.{Json, JsonNumber, JsonObject}
import io.circe.Json._

type JsonFObject[F[_]] = Map[String, JsonF[F]]

sealed trait JsonF[F[_]]

object JsonF {
  final case class JFNull[F[_]](value: F[Null]) extends JsonF[F]
  final case class JFString[F[_]](value: F[String]) extends JsonF[F]
  final case class JFNumber[F[_]](value: F[JsonNumber]) extends JsonF[F]
  final case class JFArray[F[_]](value: F[Vector[JsonF[F]]]) extends JsonF[F]
  final case class JFObject[F[_]](value: F[JsonObject]) extends JsonF[F]
  final case class JFBoolean[F[_]](value: F[Boolean]) extends JsonF[F]
}

// ----------------------------------------------------------------------------


sealed trait JsonError

object JsonError {

  final case class TypeMismatch[A, B](expected: A, got: B) extends JsonError

  def mismatch[A, B](a: A, b: B): JsonError = TypeMismatch(a, b)
}


sealed trait PathStep

object PathStep {

  final case class Key(key: String) extends PathStep
  final case class Index(index: Int) extends PathStep
}

type Path = List[PathStep]

final case class Env(path: Path, json: Json)

object Validator {
	import JsonF._
	import JsonError._
	import PathStep._

	final def apply(f: Env => Validated[JsonError, Unit]): Validator =
		Kleisli[Validated[JsonError, ?], Env, Unit](f)

	// Path => Json => Validated[JsonError, Unit]
	// type Validator = Path => Json => Validated[JsonError, Unit ] 

	type Validator = Kleisli[Validated[JsonError, ?], Env, Unit]
	
	val nullValidator: Validator = 
		apply({ 
			case Env(_, Null) => Validated.valid(())
			case Env(path, err) => Validated.invalid(mismatch(err, Null))
		})

	def atKeyValidator(key: String, validator: Validator): Validator = ???

	def objectValidator(objValidator: Vector[(String, Validator)]): Validator = 
		apply({ 
			case Env(path, obj: JsonObject) => objValidator traverse_ { case (key: String, validator: Validator) =>
				atKeyValidator(key, validator)(Env(path, obj))
			}
			case Env(path, err) => Validated.invalid(mismatch(err, Null))
		})
}

object Main {
	import Validator._

  val json = Json.fromString("1234")
  
	println(nullValidator(Env(Nil, json)))
}

Main
