package io.circe.validator

import cats.kernel.Order

sealed trait PathStep

object PathStep {
  final case class Key(key: String)  extends PathStep
  final case class Index(index: Int) extends PathStep

  implicit val orderPathStep: Order[PathStep] = Order.allEqual
}
