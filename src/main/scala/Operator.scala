package kup.knoldus.calculator

trait Operator extends Validator {

  def validateAndExecute(operands: Seq[Double]): Seq[Double] = {
    if (validate(operands)) execute(operands)
    else throw new IllegalArgumentException("Error")
  }

  protected def execute(operands: Seq[Double]): Seq[Double]

}