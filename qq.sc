import $file.jsonfmacro
import $file.jsonf

import jsonf.Validator
import jsonfmacro.JsonFLiteralMacros

import scala.language.experimental.macros

implicit class JsonFInterpolator(sc: StringContext) {
  def jsonf(args: Any*): Validator  = macro JsonFLiteralMacros.jsonfStringContext
}
