package io.circe.validator.literal

import scala.reflect.macros.blackbox

class JsontMacros(val c: blackbox.Context) extends ValidatorMacros
