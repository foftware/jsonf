package io.circe

import cats.data.{ Chain, ReaderWriterState => RWS }
import cats.mtl.implicits._

import io.circe.validator.internal.Validator._

package object validator {
  type ErrorAt = (Path, JsonError)
  type Errors = Chain[ErrorAt]
  type Path = List[PathStep]
  type Validator0[A] = RWS[Env, Errors, Unit, A]
  type Validator = Validator0[Unit]

  def trueValidator: Validator = trueValidatorF[Validator0]

  def falseValidator: Validator = falseValidatorF[Validator0]

	def objectValidator(objValidator: Vector[(String, Validator)]): Validator =
    objectValidatorF[Validator0](objValidator)

	def stringValidator(s: String): Validator = stringValidatorF[Validator0](s)

  def nullValidator: Validator = nullValidatorF[Validator0]

  def numberValidator(num: JsonNumber): Validator = numberValidatorF[Validator0](num)

  def arrayValidator(arrayValidator: Vector[Validator]): Validator = arrayValidatorF[Validator0](arrayValidator)
}
