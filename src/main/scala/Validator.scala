package kup.knoldus.calculator

trait Validator {
  def validate(operands: Seq[Double]): Boolean
}