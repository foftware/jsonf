import $ivy.`io.circe::circe-core:0.11.1`
import $ivy.`io.circe::circe-literal:0.11.1`
import $ivy.`org.typelevel::cats-core:1.6.0`
import $ivy.`org.typelevel::cats-mtl-core:0.4.0`
import $file.qq
import $plugin.$ivy.`org.spire-math::kind-projector:0.9.3`

import cats._
import cats.data.{ NonEmptyChain => Nec, _ }
import cats.implicits._
import cats.mtl.implicits._
import cats.mtl.lifting._
import cats.mtl.{ ApplicativeLocal => AL, MonadChronicle => MC }
import io.circe.{Json, JsonNumber, JsonObject}
import io.circe.Json._

sealed trait JsonError

object JsonError {

  final case class TypeMismatch[A, B](expected: A, got: B) extends JsonError
	final case class PredicateViolation[A, B](expected: A, got: B) extends JsonError
	final case class KeyNotFound(key: String, obj: JsonObject) extends JsonError

	def errorAt[F[_]](e: JsonError)(implicit MC: MC[F, Errors], L: AL[F, Env], M: Monad[F]): F[Unit] =
		L.reader(_.path) >>= (path => MC.dictate(Nec.one((path, e))))

  def mismatch0[A, B](a: A, b: B): JsonError = TypeMismatch(a, b)

	def mismatch[F[_], A, B](a: A, b: B)(implicit MC: MC[F, Errors], L: AL[F, Env], M: Monad[F]): F[Unit] = 
		errorAt(mismatch0(a, b))

	def predicateViolation0[A: Show, B: Show](a: A, b: B): JsonError = PredicateViolation(a.show, b.show)

	def predicateViolation[F[_], A: Show, B: Show](a: A, b: B)(implicit MC: MC[F, Errors], L: AL[F, Env], M: Monad[F]): F[Unit] = 
		errorAt(predicateViolation0(a, b))

	def keyNotFound0(key: String, obj: JsonObject): JsonError = KeyNotFound(key, obj)

	def keyNotFound[F[_]](a: String, b: JsonObject)(implicit MC: MC[F, Errors], L: AL[F, Env], M: Monad[F]): F[Unit] = 
		errorAt(keyNotFound0(a, b))
}

type ErrorAt = (Path, JsonError)

type Errors = Nec[ErrorAt]

sealed trait PathStep

object PathStep {

  final case class Key(key: String) extends PathStep
  final case class Index(index: Int) extends PathStep
}

type Path = List[PathStep]

final case class Env(path: Path, json: Json)

object Validator {
	import JsonError._
	import PathStep._

	def nullValidator[F[_]](implicit MC: MC[F, Errors], L: AL[F, Env], M: Monad[F]): F[Unit] = 
		L.reader(_.json) >>= {
			case Null => M.unit
			case otherwise => mismatch(Null, otherwise)
		}

	def stringValidator[F[_]](s: String)(implicit MC: MC[F, Errors], L: AL[F, Env], M: Monad[F]): F[Unit] =
		stringValidator0( s0 =>
			M.whenA(s =!= s0)(predicateViolation(s, s0))
		)

	def stringValidator0[F[_]](f: String => F[Unit])(implicit MC: MC[F, Errors], L: AL[F, Env], M: Monad[F]): F[Unit] =
		L.reader(_.json) >>= ( json =>
			json.asString.fold(mismatch("String", json))(f)
		)

	def atKeyValidator[F[_]](
		key: String, 
		validator: F[Unit]
	)(
		obj: JsonObject
	)(
		implicit 
		MC: MC[F, Errors], 
		L: AL[F, Env],
		M: Monad[F]
	): F[Unit] = 
		obj(key).fold(
			keyNotFound(key, obj)
		)(
			json => L.local { case Env(path, _) => Env(path :+ Key(key), json) }(validator)
		)

	def objectValidator[F[_]](
		objValidator: Vector[(String, F[Unit])]
	)(
		implicit
		MC: MC[F, Errors],
		L: AL[F, Env],
		M: Monad[F]
	): F[Unit] = {
		val objectValidator0: JsonObject => F[Unit] = obj => objValidator.traverse_{
			case (key, validator) => atKeyValidator(key, validator)(obj)
		}

		L.reader(_.json) >>= (json => json.asObject.fold(mismatch("Object", json))(objectValidator0))
	}
}

object Main {
	import Validator._


	// This is sooooooo painful
	// Wish there was a better way	
	lazy implicit val ev0: MC[Validator, Errors] = chronicleIorT[Reader[Env, ?], Errors]
	lazy implicit val ev1: AL[Validator, Env] = localInd[Validator, Reader[Env, ?], Env]
	lazy implicit val ev2: MonadError[Validator, Errors] = IorT.catsDataMonadErrorForIorT[Reader[Env, ?], Errors]

	// It does a little bit less stuff than MonadTrans but it is much more work...
	// http://hackage.haskell.org/package/transformers-0.5.6.2/docs/Control-Monad-Trans-Class.html
	lazy implicit val ev3: MonadLayer[Validator, Reader[Env, ?]] = new MonadLayer[Validator, Reader[Env, ?]] { 
		val innerInstance: Monad[Reader[Env, ?]] = Monad[Reader[Env, ?]]	
		val outerInstance: Monad[Validator] = Monad[Validator]

		def layer[A](inner: Reader[Env, A]): Validator[A] = IorT.liftF[Reader[Env, ?], Errors, A](inner)
		def layerImapK[A](
			ma: Validator[A]
		)(
			forward: Reader[Env, ?] ~> Reader[Env, ?],
			backward: Reader[Env, ?] ~> Reader[Env, ?]
		): Validator[A] = IorT[Reader[Env, ?], Errors, A](backward(forward(ma.value)))
	}

	type Validator[A] = IorT[Reader[Env, ?], Errors, A]

  val json = Json.obj(
		"a" -> Json.fromString("1234"),
		"b" -> Json.obj(
			"c" -> Json.fromString("4321")
		),
		"d" -> Json.Null
	)

	val validator: Validator[Unit] = objectValidator[Validator](
		Vector(
			"a" -> stringValidator("1234"),
			"b" -> objectValidator(
				Vector(
					"c" -> stringValidator("4311")
				)
			),
			"d" -> stringValidator("null")
		)
	)
  
	println(validator.value(Env(Nil, json)))
}

Main
