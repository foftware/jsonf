# jsont stands for Json Template

[![Build Status](https://travis-ci.com/foftware/jsont.svg?branch=master)](https://travis-ci.com/foftware/jsont)
[![codecov](https://codecov.io/gh/foftware/jsont/branch/master/graph/badge.svg)](https://codecov.io/gh/foftware/jsont)

## Run

```bash
$ sbt console
> jsont"""false"""
res0: io.circe.validator.Validator = cats.data.IndexedReaderWriterStateT@732368d8

> run(jsont"""null""", Json.True)
res1: cats.data.Chain[io.circe.validator.ErrorAt] = Chain(ErrorAt(List(),TypeMismatch(null,true)))

> run(jsont"""true""", Json.True)
res2: cats.data.Chain[io.circe.validator.ErrorAt] = Chain()
```

## Resouces

### Template related stuff

* http://hackage.haskell.org/package/transformers-0.5.6.2/docs/Control-Monad-Trans-Class.html
* https://typelevel.org/cats-mtl/lifting-classes.html
* https://typelevel.org/cats-mtl/mtl-classes/monadchronicle.html
* https://typelevel.org/cats-mtl/mtl-classes/functortell.html
* https://typelevel.org/cats-mtl/mtl-classes/applicativelocal.html
* http://typelevel.org/cats-mtl/api/

### Interpolator (macro) related stuff

* https://docs.scala-lang.org/overviews/macros/overview.html
* https://github.com/circe/circe/blob/master/modules/literal/src/main/scala/io/circe/literal/JsonLiteralMacros.scala
* https://github.com/circe/circe/blob/master/modules/literal/src/main/scala/io/circe/literal/package.scala
* https://github.com/circe/circe/blob/master/modules/literal/src/main/scala/io/circe/literal/LiteralInstanceMacros.scala
* https://github.com/typelevel/jawn
