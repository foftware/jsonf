// Copyright (c) 2019 Marek Kidoň and František Kocun
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package io.circe.validator

import scala.language.experimental.macros

package object literal {

  implicit class JsontInterpolator(sc: StringContext) {
    def jsont(args: Any*): Validator =
      macro JsontMacros.validatorStringContext
  }
}
