package io.circe

import cats.data.{Chain, ReaderWriterState => RWS}
import cats.mtl.implicits._


package object validator {
  type Errors        = Chain[ErrorAt]
  type Path          = List[PathStep]
  type Validator0[A] = RWS[Env, Errors, Unit, A]
  type Validator     = Validator0[Unit]


  val x = new io.circe.validator.internal.Validator[Validator0]()
  import x._

  def trueValidator: Validator = trueValidatorF

  def falseValidator: Validator = falseValidatorF

  def objectValidator(objValidator: Vector[(String, Validator)]): Validator =
    objectValidatorF(objValidator)

  def stringValidator(s: String): Validator = stringValidatorF(s)

  def nullValidator: Validator = nullValidatorF

  def numberValidator(num: JsonNumber): Validator =
    numberValidatorF(num)

  def arrayValidator(arrayValidator: Vector[Validator]): Validator =
    arrayValidatorF(arrayValidator)

  def run(validator: Validator, json: Json): Chain[ErrorAt] =
    validator.runL(Env(json), ()).value
}
