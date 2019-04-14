package io.circe.validator

import scala.language.experimental.macros

package object literal {

  implicit class ValidatorInterpolator(sc: StringContext) {
		// jsont. T for template
    def jsont(args: Any*): Validator  = macro ValidatorMacros.validatorStringContext
  }
}
