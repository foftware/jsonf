// Copyright (c) 2019 Marek Kidoň and František Kocun
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package jsont

import cats.kernel.Order

sealed trait PathStep

object PathStep {
  final case class Key(key: String)  extends PathStep
  final case class Index(index: Int) extends PathStep
  final case object Root             extends PathStep

  implicit val orderPathStep: Order[PathStep] = Order.allEqual
}
