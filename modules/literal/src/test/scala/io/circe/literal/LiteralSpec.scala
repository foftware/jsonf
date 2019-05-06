package io.circe.validator.literal

import io.circe.literal._
import io.circe.validator.{int, string, run => runValidator}

import org.scalatest.{FunSuite, Matchers}

class LiteralSpec extends FunSuite with Matchers {
  test("interpolator should validate true value") {
    runValidator(jsont"true", json"true").isEmpty shouldBe true
  }

  test("interpolator should validate false value") {
    runValidator(jsont"false", json"false").isEmpty shouldBe true
  }

  test("interpolator should validate null value") {
    runValidator(jsont"null", json"null").isEmpty shouldBe true
  }

  test("interpolator should validate integral number value") {
    runValidator(jsont""""1234"""", json""""1234"""").isEmpty shouldBe true
  }

  test("interpolator should validate decimal number value") {
    runValidator(jsont""""1234.0"""", json""""1234.0"""").isEmpty shouldBe true
  }

  test("interpolator should validate array value") {
    runValidator(
      jsont"""[1, "1234", false, true, null]""",
      json"""[1, "1234", false, true, null]"""
    ).isEmpty shouldBe true
  }

  test("interpolator should validate complex json") {
    val validator = jsont"""{
      "t": true,
      "f": false,
      "n": null,
      "o": { "b": [ 1.1, 2.2, 3.3, 4.4 ] }
    }"""

    val validated = json"""{
      "t": true,
      "f": false,
      "n": null,
      "o": { "b": [ 1.1, 2.2, 3.3, 4.4 ] }
    }"""

    runValidator(validator, validated).isEmpty shouldBe true
  }

  test("interpolator should validate complex json with arguments") {
    val validator = jsont"""{
      "a": ${int(_ > 0)},
      "b": ${string(_.length % 5 == 0)},
      "c": ${string(_.toUpperCase == "ABCD")}
    }"""

    val validated = json"""{
      "a": 1,
      "b": "55555",
      "c": "abcd"
    }"""

    runValidator(validator, validated).isEmpty shouldBe true
  }
}
