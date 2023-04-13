package kup.knoldus.calculator

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object Calculator {
  def calculate(operator: String, operands: Seq[Double]): Future[Seq[Double]] = {
    operator match {
      case "+" => execute(Add, operands)
      case "-" => execute(Subtract, operands)
      case "*" => execute(Multiply, operands)
      case "/" => execute(Divide, operands)
      case "^" => execute(Power, operands)
      case "sqrt" => execute(SquareRoot, operands)
      case "!" => execute(Factorial, operands)
      case "sum" => execute(Sum, operands)
      case "gcd" => execute(GCD, operands)
      case "odd" => execute(Odd, operands)
      case "even" => execute(Even, operands)
      case "fibonacci" => execute(Fibonacci, operands)
      case _ => throw new IllegalArgumentException("Invalid!")
    }
  }

  private def execute(operator: Operator, operands: Seq[Double]): Future[Seq[Double]] = {
    Future(operator.validateAndExecute(operands)).recoverWith { case exception: Exception => Future.failed(exception) }
  }

  def squareOfExpression(firstOperand: Double, secondOperand: Double): String = {
    val lhs = Await.result(execute(Power, Seq(firstOperand + secondOperand, 2)), Duration.Inf)
    val rhsFirst = Await.result(execute(Power, Seq(firstOperand, 2)), Duration.Inf)
    val rhsSecond = Await.result(execute(Power, Seq(secondOperand, 2)), Duration.Inf)
    val rhsThird = Await.result(execute(Power, Seq((2 * firstOperand * secondOperand), 2)), Duration.Inf)
    val rhs = rhsFirst.head + rhsSecond.head + rhsThird.head

    if (lhs.head == rhs) "Equal" else "Not Equal"

  }

  def find(numbers: Seq[Double]): Future[Seq[Double]] = {
    def conditionToFind(number: Double): Boolean = {
      val factorial = Await.result(execute(Factorial, Seq(number)), Duration.Inf)
      val power = Await.result(execute(Power, Seq(number, 6)), Duration.Inf)
      if (factorial.head == power.head) true else false
    }

    Future(numbers.filter(number => conditionToFind(number)))
  }
}