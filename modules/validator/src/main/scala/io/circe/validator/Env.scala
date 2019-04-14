package io.circe.validator

import io.circe.Json

final case class Env(path: Path, json: Json)

object Env {
  final def apply(json: Json): Env = Env(Nil, json)
}
