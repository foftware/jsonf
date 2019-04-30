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
    case PredicateViolation(reason) =>
      Json.obj(
        "kind" -> Json.fromString("validation error"),
        "reason" -> Json.fromString(reason)
      )
    case KeyNotFound(key, obj) =>
      Json.obj(
        "kind" -> Json.fromString("key not found"),
        "reason" -> Json.fromString(s"$key in $obj")
      )
    case TypeMismatch(expected, got) =>
      Json.obj(
        "kind" -> Json.fromString("type mismatch"),
        "reason" -> Json.fromString(s"expected: $expected, got: $got")
      )
    //FIXME
    case _ => ???
  }

  implicit val errorAtEncoder: Encoder[ErrorAt] = Encoder.instance ( e =>
    Json.obj(
      "at" -> e.at.asJson,
      "error" -> e.error.asJson
    )
  )

  val jsonValidatorPrettifier: Prettifier = new Prettifier {
    final def apply(o: Any): String = {
      // Ugly but that's how Prettifier works...
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
