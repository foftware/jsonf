package io.circe.validator

import cats.tests.CatsSuite
import cats.data.Chain
import io.circe.Json

import io.circe.validator.JsonError.TypeMismatch

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

}
