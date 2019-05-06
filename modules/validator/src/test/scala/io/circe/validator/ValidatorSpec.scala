package io.circe.validator

import cats.tests.CatsSuite
import cats.data.Chain
import io.circe.{Json, JsonNumber, JsonObject}

import io.circe.validator.JsonError.{
  PredicateViolation,
  TypeMismatch,
  KeyNotFound
}
import io.circe.validator.PathStep.{Index, Key}

trait Runner {
  def runValidator = run _
}

class ValidatorSpec extends CatsSuite with Runner {

  test("pass should always succeed") {
    val actual   = runValidator(pass, Json.Null)
    val expected = Chain.empty

    actual shouldBe expected
  }

  test("failed should always fail") {
    val actual = runValidator(failed, Json.Null)
    val expected =
      Chain(ErrorAt(List(), PredicateViolation("Fail unconditionally")))

    actual shouldBe expected
  }

  test("nullValidator should succeed on Null") {
    val actual   = runValidator(nullValidator, Json.Null)
    val expected = Chain.empty

    actual shouldBe expected
  }

  test("nullValidator should fail on values other than Null") {
    val actual   = runValidator(nullValidator, Json.False)
    val expected = Chain.one(ErrorAt(List.empty, TypeMismatch("null", "false")))

    actual should ===(expected)
  }

  test("trueValidator should succeed on True") {
    val actual   = runValidator(trueValidator, Json.True)
    val expected = Chain.empty

    actual shouldBe expected
  }

  test("trueValidator should fail on values other than True") {
    val actual   = runValidator(trueValidator, Json.False)
    val expected = Chain.one(ErrorAt(List.empty, TypeMismatch("true", "false")))

    actual should ===(expected)
  }

  test("falseValidator should succeed on False") {
    val actual   = runValidator(falseValidator, Json.False)
    val expected = Chain.empty

    actual shouldBe expected
  }

  test("falseValidator should fail on values other than False") {
    val actual   = runValidator(falseValidator, Json.True)
    val expected = Chain.one(ErrorAt(List.empty, TypeMismatch("false", "true")))

    actual shouldBe expected
  }

  test("string should succeed if given String predicate succeeds") {
    val actual   = runValidator(string(_ == "1234"), Json.fromString("1234"))
    val expected = Chain.empty

    actual shouldBe expected
  }

  test("string should fail if given String predicate fails") {
    val actual = runValidator(string(_ == "1234"), Json.fromString("4321"))
    val expected = Chain(
      ErrorAt(
        List(),
        PredicateViolation(
          "String value 4321 does not satisfy the given predicate."
        )
      )
    )

    actual shouldBe expected
  }

  test("number should succeed if given Number predicate succeeds") {
    val jsonNumber = JsonNumber.fromDecimalStringUnsafe("1234")
    val actual     = runValidator(number(_ == jsonNumber), Json.fromInt(1234))
    val expected   = Chain.empty

    actual shouldBe expected
  }

  test("number should fail if given Number predicate fails") {
    val jsonNumber = JsonNumber.fromIntegralStringUnsafe("1234")
    val actual     = runValidator(number(_ == jsonNumber), Json.fromInt(4321))
    val expected = Chain(
      ErrorAt(
        List(),
        PredicateViolation(
          "Number value 4321 does not satisfy the given predicate."
        )
      )
    )

    actual shouldBe expected
  }

  test("int should succeeed if given Int predicate succeeds") {
    val actual   = runValidator(int(_ > 4320), Json.fromInt(4321))
    val expected = Chain()

    actual shouldBe expected
  }

  test("int should fail if given Int predicate fails") {
    val actual = runValidator(int(_ < 4320), Json.fromInt(4321))
    val expected = Chain(
      ErrorAt(
        List(),
        PredicateViolation(
          "Number value 4321 does not satisfy the given predicate."
        )
      )
    )

    actual shouldBe expected
  }

  test("bigInt should succeeed if given BigInt predicate succeeds") {
    val actual   = runValidator(bigInt(_ > 4320), Json.fromBigInt(4321))
    val expected = Chain()

    actual shouldBe expected
  }

  test("bigInt should fail if given BigInt predicate fails") {
    val actual = runValidator(bigInt(_ < 4320), Json.fromBigInt(4321))
    val expected = Chain(
      ErrorAt(
        List(),
        PredicateViolation(
          "Number value 4321 does not satisfy the given predicate."
        )
      )
    )

    actual shouldBe expected
  }

  test("bigDecimal should succeeed if given BigDecimal predicate succeeds") {
    val predicate = bigDecimal(_ > BigDecimal.decimal(4320))
    val actual =
      runValidator(predicate, Json.fromBigDecimal(BigDecimal.decimal(4321)))
    val expected = Chain()

    actual shouldBe expected
  }

  test("long should succeeed if given Long predicate succeeds") {
    val actual   = runValidator(long(_ > 4320), Json.fromLong(4321))
    val expected = Chain()

    actual shouldBe expected
  }

  test("long should fail if given Long predicate fails") {
    val actual = runValidator(long(_ < 4320), Json.fromLong(4321))
    val expected = Chain(
      ErrorAt(
        List(),
        PredicateViolation(
          "Number value 4321 does not satisfy the given predicate."
        )
      )
    )

    actual shouldBe expected
  }

  test("double should succeeed if given Double predicate succeeds") {
    val actual   = runValidator(double(_ > 4320.0), Json.fromDoubleOrNull(4321.0))
    val expected = Chain()

    actual shouldBe expected
  }

  test("double should fail if given Double predicate fails") {
    val actual = runValidator(long(_ < 4320.0), Json.fromDoubleOrNull(4321.0))
    val expected = Chain(
      ErrorAt(
        List(),
        PredicateViolation(
          "Number value 4321.0 does not satisfy the given predicate."
        )
      )
    )

    actual shouldBe expected
  }

  test("bigDecimal should fail if given BigDecimal predicate fails") {
    val actual = runValidator(bigInt(_ < 4320), Json.fromInt(4321))
    val expected = Chain(
      ErrorAt(
        List(),
        PredicateViolation(
          "Number value 4321 does not satisfy the given predicate."
        )
      )
    )

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
    val actual    = runValidator(validator, Json.arr())
    val expected = Chain(
      ErrorAt(
        List(),
        PredicateViolation("Array has less elements (0) than expected (1)")
      )
    )

    actual shouldBe expected
  }

  test("objectValidator should succeed of all of its keys succeed") {
    val validator = objectValidator(
      Vector("a" -> trueValidator, "b" -> falseValidator)
    )
    val json     = Json.obj("a" -> Json.True, "b" -> Json.False)
    val actual   = runValidator(validator, json)
    val expected = Chain.empty

    actual shouldBe expected
  }

  test("objectValidator should fail of any of its keys fail") {
    val validator = objectValidator(Vector("a" -> trueValidator))
    val actual    = runValidator(validator, Json.obj("a" -> Json.False))
    val expected  = Chain(ErrorAt(List(Key("a")), TypeMismatch("true", "false")))

    actual shouldBe expected
  }

  test("objectValidator should fail of any of its validated keys are missing") {
    val validator = objectValidator(Vector("a" -> trueValidator))
    val actual    = runValidator(validator, Json.obj())
    val expected  = Chain(ErrorAt(List(), KeyNotFound("a", JsonObject.empty)))

    actual shouldBe expected
  }
}
