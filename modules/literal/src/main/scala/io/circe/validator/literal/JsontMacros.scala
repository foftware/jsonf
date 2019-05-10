// Copyright (c) 2019 Marek Kidoň and František Kocun
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package io.circe.validator.literal

import scala.reflect.macros.blackbox

class JsontMacros(val c: blackbox.Context) extends ValidatorMacros
