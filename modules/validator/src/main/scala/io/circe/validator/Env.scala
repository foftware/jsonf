package io.circe.validator

import io.circe.Json
import io.circe.validator.PathStep.{Index, Key, Root}

final case class Env(path: Path, json: Json)

object Env {
  final def apply(json: Json): Env = Env(List(Root), json)

  final private[validator] def index(index: Int, json: Json): Env => Env = {
    case Env(path, _) => Env(path :+ Index(index), json)
  }

  final private[validator] def key(key: String, json: Json): Env => Env = {
    case Env(path, _) => Env(path :+ Key(key), json)
  }
}
