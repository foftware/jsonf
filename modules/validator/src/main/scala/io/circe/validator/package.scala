// Copyright (c) 2019 Marek Kidoň and František Kocun
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package io.circe

import cats.data.{Chain, ReaderWriterState => RWS}
import cats.mtl.implicits._
import io.circe.validator.internal.ValidatorF

package object validator extends ValidatorF[RWS[Env, Chain[ErrorAt], Unit, ?]] {
  type Errors        = Chain[ErrorAt]
  type Path          = List[PathStep]
  type Validator0[A] = RWS[Env, Errors, Unit, A]
  type Validator     = Validator0[Unit]

  def run(validator: Validator, json: Json): Chain[ErrorAt] =
    validator.runL(Env(json), ()).value
}
