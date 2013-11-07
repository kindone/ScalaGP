package kindone.scalagp


abstract class BinaryOperation
{
	def left:Expression
	def right:Expression
	lazy val subtreeSize = left.subtreeSize + right.subtreeSize
}

case class Add(left:Expression, right:Expression) extends BinaryOperation {
	def apply() = left() + right()
}

case class Subtract(left:Expression, right:Expression) extends BinaryOperation {
	def apply() = left() - right()
} 

case class Multiply(left:Expression, right:Expression) extends BinaryOperation {
	def apply() = left() * right()
}

case class Divide(left:Expression, right:Expression) extends BinaryOperation {
	def apply() = if(scala.math.abs(right()) <= 0.1E-6) 0.0 else left() / right()
}

case class Modular(left:Expression, right:Expression) extends BinaryOperation {
	def apply() = left() % right()
}

case class Pow(left:Expression, right:Expression) extends BinaryOperation {
	def apply() = Math.pow(left(), right())
}



case class Or(left:Expression, right:Expression) extends BinaryOperation {
	def apply() = left().toInt | right().toInt
}

case class And(left:Expression, right:Expression) extends BinaryOperation {
	def apply() = left().toInt & right().toInt
}

case class Xor(left:Expression, right:Expression) extends BinaryOperation {
	def apply() = left().toInt & right().toInt
}

case class ShiftLeft(left:Expression, right:Expression) extends BinaryOperation {
	def apply() = left().toInt << right().toInt
}

case class ShiftRight(left:Expression, right:Expression) extends BinaryOperation {
	def apply() = left().toInt >> right().toInt
}

case class Equals(left:Expression, right:Expression) extends BinaryOperation {
	def apply() = if(left() == right()) 1.0 else 0.0
}

case class NotEquals(left:Expression, right:Expression) extends BinaryOperation {
	def apply() = if(left() != right()) 1.0 else 0.0
}

case class GreaterThanOrEquals(left:Expression, right:Expression) extends BinaryOperation {
	def apply() = if(left() >= right()) 1.0 else 0.0
}

case class LessThanOrEquals(left:Expression, right:Expression) extends BinaryOperation {
	def apply() = if(left() <= right()) 1.0 else 0.0
}

case class GreaterThan(left:Expression, right:Expression) extends BinaryOperation {
	def apply() = if(left() > right()) 1.0 else 0.0
}

case class LessThan(left:Expression, right:Expression) extends BinaryOperation {
	def apply() = if(left() < right()) 1.0 else 0.0
}

// vim: set ts=4 sw=4 et:
