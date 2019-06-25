[![Build Status](https://travis-ci.com/foftware/jsont.svg?branch=master)](https://travis-ci.com/foftware/jsont)
[![codecov](https://codecov.io/gh/foftware/jsont/branch/master/graph/badge.svg)](https://codecov.io/gh/foftware/jsont)
[![Gitter chat](https://badges.gitter.im/fun-software/gitter.png)](https://gitter.im/fun-software/jsont)

Function-based Json templating language

## Overview

`jsont` is a combinator based library for building Json validators.

## Example

Most convenient way to describe your Json is to use one of the supplied
string interpolators. You can use either the `jsont`:

```
import jsont.run
import jsont.literal._

val validator = jsont"""{
  "a": ${int(_ > 0)},
  "b": ${string(_.length % 5 == 0)},
  "c": [
    ${string(_.toUpperCase == "ABCD")},
    ${regex("""\d\d\d\d-\d\d-\d\d""".r)}
  ]
}"""

val validated = json"""{
  "a": 1,
  "b": "55555",
  "c": [
    "abcd",
    "2019-12-12"
  ]
}"""

run(validator, validated)
// Chain()
```

Or when used in tests, use `scalatest`'s *Matcher* directly:

```
jsont"""{
  "a": ${int(_ > 0)},
  "b": ${string(_.length % 5 == 0)},
  "c": [
    ${string(_.toUpperCase == "ABCD")},
    ${regex("""\d\d\d\d-\d\d-\d\d""".r)}
  ]
}""" should matchJson """{
  "a": 1,
  "b": "55555",
  "c": [
    "abcd",
    "2019-12-12"
  ]
}"""
```

## Getting started

Add the following to your `build.sbt`:

```
libraryDependencies += "com.github.foftware" % "jsont-core_2.12" % "0.1.1"
libraryDependencies += "com.github.foftware" % "jsont-literal_2.12" % "0.1.1"
libraryDependencies += "com.github.foftware" % "jsont-scalatest_2.12" % "0.1.1"
```
