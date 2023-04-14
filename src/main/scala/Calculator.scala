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
    val futureOfLHS = for {
      expression1 <- execute(Power, Seq(firstOperand + secondOperand, 2))
    } yield expression1.head

    val futureOfRHS = for {
      expression2 <- execute(Power, Seq(firstOperand, 2))
      expression3 <- execute(Power, Seq(secondOperand, 2))
      expression4 <- execute(Power, Seq((2 * firstOperand * secondOperand), 2))
    } yield expression2.head + expression3.head + expression4.head

    val lhs = Await.result(futureOfLHS, Duration.Inf)
    val rhs = Await.result(futureOfRHS, Duration.Inf)

    if (lhs == rhs) "Equal" else "Not Equal"
  }

  def find(numbers: Seq[Double]): Future[Seq[Double]] = {
    def conditionToFind(number: Double): Boolean = {
      val factorial = Await.result(execute(Factorial, Seq(number)), Duration.Inf)
      val power = Await.result(execute(Power, Seq(number, 6)), Duration.Inf)
      if (factorial.head == power.head) true else false
    }

    Future(numbers.filter(number => conditionToFind(number)))
  }

  def findAverageAfterChainingOperations(numbers: Seq[Double]): Future[Double] = {
    val futures = numbers.map { number =>
      execute(Fibonacci, Seq(number))
        .flatMap(result => execute(Odd, result))
        .map(result => result.reduce(_ + _))
    }

    Future.sequence(futures).map(_.reduce(_ + _) / numbers.length)
  }
}