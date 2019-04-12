import $file.jsonfmacro

import jsonfmacro.JsonFLiteralMacros

import scala.language.experimental.macros

implicit class JsonFInterpolator(sc: StringContext) {
  def jsonf(args: Any*): String  = macro JsonFLiteralMacros.jsonfStringContext
}
