// Copyright (c) 2019 Marek Kidoň and František Kocun
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package org.scalatest

import io.circe.Json
import jsont.{Validator, run => runValidator}
import org.scalatest.JsonValidatorPrettifier.jsonValidatorPrettifier
import org.scalatest.matchers.{MatchResult, Matcher}

trait JsonValidatorMatchers {

  private class JsonPassesValidation(val right: Validator)
      extends Matcher[Json] {
    final def apply(left: Json): MatchResult = {
      val errors = runValidator(right, left)

      MatchResult(
        errors.isEmpty,
        s"Json did not pass validation ${jsonValidatorPrettifier(errors)}",
        s"Json did pass validation"
      )
    }
  }

  def passValidation(validator: Validator): Matcher[Json] =
    new JsonPassesValidation(validator)

  implicit class MatchJsonInterpolator(sc: StringContext) {
    def matchJson(args: Any*): Matcher[Json] =
      macro MatchJsonMacros.jsonMatcherStringContext
  }
}
