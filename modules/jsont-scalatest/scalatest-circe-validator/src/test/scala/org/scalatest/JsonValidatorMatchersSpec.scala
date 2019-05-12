package org.scalatest

import io.circe.literal._

class JsonValidatorMatchersSpec
    extends Matchers
    with JsonValidatorMatchers
    with WordSpecLike {

  "JsonValidatorMatchers" should {
    "succeed" in {
      json"""{ "a": true }""" should matchJson"""{ "a": true }"""
      json"""{ "a": false }""" should matchJson"""{ "a": false }"""
      json"""{ "a": null }""" should matchJson"""{ "a": null }"""
      json"""{ "a": 123 }""" should matchJson"""{ "a": 123 }"""
      json"""{ "a": "x" }""" should matchJson"""{ "a": "x" }"""
      json"""{ "a": [1] }""" should matchJson"""{ "a": [1] }"""
      json"""{ "a": { "b": 1 } }""" should matchJson"""{ "a": { "b": 1 } }"""
    }

    "fail" in {
      json"""{ "a": true }""" shouldNot matchJson"""{ "a": 1 }"""
    }
  }
}
