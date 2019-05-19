[![Build Status](https://travis-ci.com/foftware/jsont.svg?branch=master)](https://travis-ci.com/foftware/jsont)
[![codecov](https://codecov.io/gh/foftware/jsont/branch/master/graph/badge.svg)](https://codecov.io/gh/foftware/jsont)
[![Gitter chat](https://badges.gitter.im/fun-software/gitter.png)](https://gitter.im/fun-software/jsont)

# Jsont

## Overview

Jsont is a library that provides function-based Json templating language.

## Example

To describe your Json you can use either the `jsont` string interpolator:

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

Or when used in tests, use `scalatest` *Matcher* directly:

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

TODO: libraryDependencies

## Documentation

TODO: javadoc.io ?
