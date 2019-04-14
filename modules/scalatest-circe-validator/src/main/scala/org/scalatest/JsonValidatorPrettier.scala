package org.scalatest

import cats.implicits.catsKernelStdOrderForList
import io.circe.{ Encoder, Json }
import io.circe.syntax._
import io.circe.validator.{ ErrorAt, Errors, Path }
import io.circe.validator.JsonError
import io.circe.validator.JsonError.{ KeyNotFound, TypeMismatch, PredicateViolation }
import org.scalactic.Prettifier

object JsonValidatorPrettifier {
  implicit val pathEncoder: Encoder[Path] = Encoder.instance(Function.const(Json.fromString("path")))

  implicit val jsonErrorEncoder: Encoder[JsonError] = Encoder.instance {
    case PredicateViolation(expected@_, got@_) =>
      Json.obj( "kind" -> "validation error".asJson, "reason" -> "".asJson )
    case KeyNotFound(key, obj) =>
      Json.obj("kind" -> "key not found".asJson, "reason" -> s"$key in $obj".asJson )
    case TypeMismatch(expected, got) =>
      Json.obj( "kind" -> "type mismatch".asJson, "reason" -> s"expected: $expected, got: $got".asJson )
  }

  implicit val errorAtEncoder: Encoder[ErrorAt] = Encoder.instance ( e =>
    Json.obj(
      "at" -> e.at.asJson,
      "error" -> e.error.asJson
    )
  )

  val jsonValidatorPrettifier: Prettifier = new Prettifier {
    final def apply(o: Any): String = {
      val errors = o.asInstanceOf[Errors]
      errors
        .groupBy(_.at)
        .mapValues(_.asJson)
        .values
        .asJson
        .toString
    }
  }
}
