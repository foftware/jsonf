package io.circe.validator

import scala.language.experimental.macros

package object literal {

  implicit class JsontInterpolator(sc: StringContext) {
    def jsont(args: Any*): Validator =
      macro JsontMacros.validatorStringContext
  }
}
