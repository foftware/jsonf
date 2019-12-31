// Copyright (c) 2019 Marek Kidoň and František Kocun
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

import cats.data.{Chain, NonEmptyChain => Nec, IorT, Reader}
import cats.mtl.implicits._
import io.circe.Json
import jsont.internal.ValidatorF
import cats.data.Ior.{Both, Left, Right}

package object jsont extends ValidatorF[IorT[Reader[Env, ?], Nec[ErrorAt], ?]] {
  type Errors        = Nec[ErrorAt]
  type Path          = List[PathStep]
  type Validator0[A] = IorT[Reader[Env, ?], Nec[ErrorAt], A]
  type Validator     = Validator0[Unit]

  def run(validator: Validator, json: Json): Chain[ErrorAt] =
    validator.value.run(Env(json)) match {
      case Both(a, _) => a.toChain
      case Left(a)    => a.toChain
      case Right(_)   => Chain.empty
    }
}
