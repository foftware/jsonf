// Copyright (c) 2019 Marek Kidoň and František Kocun
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package jsont

import io.circe.Json
import jsont.PathStep.{Index, Key, Root}
import jsont.PathStep.Root

final case class Env(path: Path, json: Json)

object Env {
  final def apply(json: Json): Env = Env(List(Root), json)

  final private[jsont] def index(index: Int, json: Json): Env => Env = {
    case Env(path, _) => Env(path :+ Index(index), json)
  }

  final private[jsont] def key(key: String, json: Json): Env => Env = {
    case Env(path, _) => Env(path :+ Key(key), json)
  }
}
