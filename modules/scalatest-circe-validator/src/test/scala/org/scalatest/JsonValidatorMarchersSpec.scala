package org.scalatest

import io.circe.Json
import io.circe.validator.trueValidator
// import org.scalatest.matchers.MatchResult

class JsonValidatorMatchersSpec
    extends Matchers
    with JsonValidatorMatchers
    with WordSpecLike {

  "JsonValidatorMatchers" should {
    "succeed" when {
      "validation passes" in {
        val actual = passValidation(trueValidator)(Json.True)

        actual.matches shouldBe true
      }
    }

    "fail" when {
      "validation did not pass" in {
        val actual = passValidation(trueValidator)(Json.False)

        actual.matches shouldBe false
      }
    }
  }
}
