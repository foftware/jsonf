package org.scalatest

import io.circe.Json
import io.circe.validator.literal.ValidatorMacrosTrait
import io.circe.validator.{Validator, run => runValidator}
import org.scalatest.JsonValidatorPrettifier.jsonValidatorPrettifier
import org.scalatest.matchers.{MatchResult, Matcher}

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

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

class MatchJsonMacros(val c: blackbox.Context)
    extends ValidatorMacrosTrait
    with JsonValidatorMatchers {
  import c.universe._

  final def jsonMatcherStringContext(
      args: c.Expr[Any]*
  ): c.Expr[Matcher[Json]] =
    c.Expr[Matcher[Json]](
      q"passValidation(${validatorStringContextTree(args: _*)})"
    )
}
