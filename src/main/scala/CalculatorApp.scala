package kup.knoldus.calculator

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

object CalculatorApp extends App {

  Calculator.calculate("+", Seq(1, 2)).onComplete {
    case Success(value) => println(value)
    case Failure(exception) => println(exception)
  }

  Calculator.calculate("gcd", Seq(50, 25)).onComplete {
    case Success(value) => println(value)
    case Failure(exception) => println(exception)
  }
  Calculator.calculate("/", Seq(1, 2)).onComplete {
    case Success(value) => println(value)
    case Failure(exception) => println(exception)
  }
  Calculator.calculate("-", Seq(10, 11)).onComplete {
    case Success(value) => println(value)
    case Failure(exception) => println(exception)
  }
  Calculator.calculate("*", Seq(6, 9)).onComplete {
    case Success(value) => println(value)
    case Failure(exception) => println(exception)
  }

  Calculator.calculate("odd", Seq(6, 9, 2, 64, 99, 100)).onComplete {
    case Success(value) => println(value)
    case Failure(exception) => println(exception)
  }

  Calculator.calculate("even", Seq(11, 12, 13, 14)).onComplete {
    case Success(value) => println(value)
    case Failure(exception) => println(exception)
  }

  Calculator.calculate("fibonacci", Seq(10)).onComplete {
    case Success(value) => println(value)
    case Failure(exception) => println(exception)
  }

  val resultOfSquareOfExpression = Calculator.squareOfExpression(1.0, 2.0)
  val resultOfFind = Await.result(Calculator.find(Seq(1, 2, 3)), Duration.Inf)

  println(resultOfSquareOfExpression)
  println(resultOfFind)
}