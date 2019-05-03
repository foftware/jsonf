package io.circe.validator

import cats.tests.CatsSuite
import cats.data.Chain
import io.circe.Json

import io.circe.validator.JsonError.{PredicateViolation, TypeMismatch}
import io.circe.validator.PathStep.Index

trait Runner {
  def runValidator = run _
}

class ValidatorSpec extends CatsSuite with Runner {

  test("nullValidator should succeed on Json Null value") {
    val actual   = runValidator(nullValidator, Json.Null)
    val expected = Chain.empty

    actual shouldBe expected
  }

  test("nullValidator should fail on values other than Json Null") {
    val actual   = runValidator(nullValidator, Json.False)
    val expected = Chain.one(ErrorAt(List.empty, TypeMismatch("null", "false")))

    actual should ===(expected)
  }

  test("trueValidator should succeed on Json True value") {
    val actual   = runValidator(trueValidator, Json.True)
    val expected = Chain.empty

    actual shouldBe expected
  }

  test("trueValidator should fail on values other than Json True") {
    val actual   = runValidator(trueValidator, Json.False)
    val expected = Chain.one(ErrorAt(List.empty, TypeMismatch("true", "false")))

    actual should ===(expected)
  }

  test("falseValidator should succeed on Json False value") {
    val actual   = runValidator(falseValidator, Json.False)
    val expected = Chain.empty

    actual shouldBe expected
  }

  test("falseValidator should fail on values other than Json False") {
    val actual   = runValidator(falseValidator, Json.True)
    val expected = Chain.one(ErrorAt(List.empty, TypeMismatch("false", "true")))

    actual shouldBe expected
  }

  test("arrayValidator should succeed if all its elements succeed") {
    val validator = arrayValidator(Vector(trueValidator, falseValidator))
    val validated = Json.arr(Json.True, Json.False)
    val actual    = runValidator(validator, validated)
    val expected  = Chain.empty

    actual shouldBe expected
  }

  test("arrayValidator should fail if any if its elemets fail") {
    val validator = arrayValidator(Vector(trueValidator, falseValidator))
    val validated = Json.arr(Json.True, Json.True)
    val actual    = runValidator(validator, validated)
    val expected  = Chain(ErrorAt(List(Index(1)), TypeMismatch("false", "true")))

    actual shouldBe expected
  }

  test("arrayValidator should fail if it validates less elements than expected") {
    val validator = arrayValidator(Vector(trueValidator))
    val validated = Json.arr()
    val actual    = runValidator(validator, validated)
    val expected = Chain(
      ErrorAt(
        List(),
        PredicateViolation("Array has less elements (0) than expected (1)")
      )
    )

    actual shouldBe expected
  }
}
