// Copyright (c) 2019 Marek Kidoň and František Kocun
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package org.scalatest

import io.circe.Json
import io.circe.validator.literal.ValidatorMacros
import org.scalatest.matchers.Matcher
import scala.reflect.macros.blackbox

class MatchJsonMacros(val c: blackbox.Context)
    extends ValidatorMacros
    with JsonValidatorMatchers {
  import c.universe._

  final def jsonMatcherStringContext(
      args: c.Expr[Any]*
  ): c.Expr[Matcher[Json]] =
    c.Expr[Matcher[Json]](
      q"passValidation(${validatorStringContextTree(args: _*)})"
    )
}
