package io.circe

import cats.data.{Chain, ReaderWriterState => RWS}
import cats.mtl.implicits._
import io.circe.validator.{Env, ErrorAt, PathStep}
import io.circe.validator.internal.ValidatorF

object validatorTypes {
  type Errors        = Chain[ErrorAt]
  type Path          = List[PathStep]
  type Validator0[A] = RWS[Env, Errors, Unit, A]
}

package object validator extends ValidatorF[validatorTypes.Validator0] {
  type Errors        = validatorTypes.Errors
  type Path          = validatorTypes.Path
  type Validator0[A] = validatorTypes.Validator0[A]
  type Validator     = Validator0[Unit]

  def run(validator: Validator, json: Json): Chain[ErrorAt] =
    validator.runL(Env(json), ()).value
}
