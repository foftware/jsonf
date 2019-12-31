package org.scalatest


import org.scalatest.wordspec.AnyWordSpecLike

import io.circe.generic.semiauto.deriveDecoder
import io.circe.literal._
import io.circe.Decoder

import jsont.{as, int}

class JsonValidatorMatchersSpec
    extends org.scalatest.matchers.should.Matchers
    with JsonValidatorMatchers
    with AnyWordSpecLike {

  "JsonValidatorMatchers" should {
    "succeed" in {
      json"""{ "a": true }""" should matchJson"""{ "a": true }"""
      json"""{ "a": false }""" should matchJson"""{ "a": false }"""
      json"""{ "a": null }""" should matchJson"""{ "a": null }"""
      json"""{ "a": 123 }""" should matchJson"""{ "a": 123 }"""
      json"""{ "a": "x" }""" should matchJson"""{ "a": "x" }"""
      json"""{ "a": [1] }""" should matchJson"""{ "a": [1] }"""
      json"""{ "a": { "b": 1 } }""" should matchJson"""{ "a": { "b": 1 } }"""
      json"""{ "a": 123 }""" should matchJson"""{ "a": ${int(_ > 3)} }"""
    }

    "fail" in {
      json"""{ "a": true }""" shouldNot matchJson"""{ "a": 1 }"""
    }
  }

  "decoder validator" should {
    final case class User(name: String, age: Int)
    implicit val userDecoder: Decoder[User] = deriveDecoder

    "succeed" in {
      json"""{ "a": 123, "u": { "name": "boris", "age": 41 } }""" should matchJson"""{ "u": ${as[
        User
      ](u => u.name.length < 20 && u.age == 41)} }"""
    }

    "fail" in {
      json"""{ "a": 123, "u": { "name": "boris", "age": 41 } }""" shouldNot matchJson"""{ "u": ${as[
        User
      ](u => u.name.length < 20 && u.age == 42)} }"""
    }
  }
}
