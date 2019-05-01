package org.scalatest

import io.circe.validator.{Validator, run => runValidator}
import io.circe.Json
import org.scalatest.matchers.{Matcher, MatchResult}
import org.scalatest.JsonValidatorPrettifier.jsonValidatorPrettifier

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
}
