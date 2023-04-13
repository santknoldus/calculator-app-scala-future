package kup.knoldus.calculator

import scala.annotation.tailrec

object Add extends Operator {
  override def validate(operands: Seq[Double]): Boolean =
    if (operands.size == 2) true else false

  override protected def execute(operands: Seq[Double]): Seq[Double] =
    Seq(operands.reduce(_ + _))
}

object Subtract extends Operator {
  override def validate(operands: Seq[Double]): Boolean =
    if (operands.size == 2) true else false

  override protected def execute(operands: Seq[Double]): Seq[Double] =
    Seq(operands.reduce(_ - _))
}

object Multiply extends Operator {
  override def validate(operands: Seq[Double]): Boolean =
    if (operands.size == 2) true else false

  override protected def execute(operands: Seq[Double]): Seq[Double] =
    Seq(operands.reduce(_ * _))
}

object Divide extends Operator {
  override def validate(operands: Seq[Double]): Boolean =
    if (operands.size == 2) {
      if (operands.last > 0) true else false
    }
    else false

  override protected def execute(operands: Seq[Double]): Seq[Double] =
    Seq(operands.reduce(_ / _))
}

object Power extends Operator {
  override def validate(operands: Seq[Double]): Boolean =
    if (operands.size == 2)
      if (operands.last == operands.last.toInt) true else false
    else false

  override protected def execute(operands: Seq[Double]): Seq[Double] = {
    @tailrec
    def powerOfNumber(number: Double, times: Double): Double = {
      if (times == 0) number
      else powerOfNumber(number * number, times - 1)
    }

    if (operands.head == 0) Seq(1.0)
    else Seq(powerOfNumber(operands.head, operands.last))
  }
}

object SquareRoot extends Operator {
  override def validate(operands: Seq[Double]): Boolean = {
    if (operands.size == 1)
      if (operands.head >= 0) true else false
    else false
  }

  override protected def execute(operands: Seq[Double]): Seq[Double] = {
    Seq(Math.sqrt(operands.head))
  }
}

object Factorial extends Operator {
  override def validate(operands: Seq[Double]): Boolean = {
    if (operands.size == 1)
      if (operands.head >= 0 && operands.head == operands.head.toInt) true else false
    else false
  }

  override protected def execute(operands: Seq[Double]): Seq[Double] = {
    @tailrec
    def findFactorial(number: Double, factorial: Double): Double = {
      if (number == 0) factorial
      else findFactorial(number - 1, factorial * number)
    }

    Seq(findFactorial(operands.head, 1))
  }
}

//to find sum of all elements of sequence
object Sum extends Operator {
  override def validate(operands: Seq[Double]): Boolean =
    if (operands.nonEmpty) true else false

  override protected def execute(operands: Seq[Double]): Seq[Double] = {
    Seq(operands.reduce(_ + _))
  }
}

object GCD extends Operator {
  override def validate(operands: Seq[Double]): Boolean =
    if (operands.size == 2) true else false

  override protected def execute(operands: Seq[Double]): Seq[Double] = {
    @tailrec
    def gcd(firstNumber: Double, secondNumber: Double): Double = {
      if (secondNumber == 0)
        firstNumber
      else gcd(secondNumber, firstNumber % secondNumber)
    }

    Seq(gcd(operands.head, operands.last))
  }
}

//to filter odd elements from sequence
object Odd extends Operator {
  override def validate(operands: Seq[Double]): Boolean =
    if (operands.nonEmpty) true else false

  override protected def execute(operands: Seq[Double]): Seq[Double] = {
    operands.filterNot(_ % 2 == 0)
  }
}

//to filter even elements from list
object Even extends Operator {
  override def validate(operands: Seq[Double]): Boolean =
    if (operands.nonEmpty) true else false

  override protected def execute(operands: Seq[Double]): Seq[Double] = {
    operands.filter(_ % 2 == 0)
  }
}

//to generate fibonacci series
object Fibonacci extends Operator {
  override def validate(operands: Seq[Double]): Boolean = {
    if (operands.size == 1)
      if (operands.head >= 0 && operands.head == operands.head.toInt) true else false
    else false
  }

  override protected def execute(operands: Seq[Double]): Seq[Double] = {
    @tailrec
    def fibonacci(number: Double, first: Double, second: Double, list: Seq[Double]): Seq[Double] = {
      if (number == 0) list
      else fibonacci(number - 1, second, second + first, list :+ first)
    }

    fibonacci(operands.head, 0, 1, Seq())
  }
}