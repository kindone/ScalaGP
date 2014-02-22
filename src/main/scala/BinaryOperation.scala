package kindone.scalagp

abstract class BinaryOperation extends Operation {
	def left: Expression
	def right: Expression
	lazy val size = left.size + right.size + 1
	def subexpression(pos: Int): Expression = {
		if (pos >= size - 1)
			this
		else if (pos == size - 2)
			right
		else if (pos >= left.size)
			right.subexpression(pos - left.size)
		else if (pos == left.size - 1)
			left
		else
			left.subexpression(pos)
	}

	def copy(l: Expression, r: Expression): Expression

}

case class Add(left: Expression, right: Expression) extends BinaryOperation {
	def apply() = left() + right()
	def copy(l: Expression, r: Expression) = Add(l, r)
}

case class Subtract(left: Expression, right: Expression) extends BinaryOperation {
	def apply() = left() - right()
	def copy(l: Expression, r: Expression) = Subtract(l, r)
}

case class Multiply(left: Expression, right: Expression) extends BinaryOperation {
	def apply() = left() * right()
	def copy(l: Expression, r: Expression) = Multiply(l, r)
}

case class Divide(left: Expression, right: Expression) extends BinaryOperation {
	def apply() = if (scala.math.abs(right()) <= 0.1E-6) 0.0 else left() / right()
	def copy(l: Expression, r: Expression) = Divide(l, r)
}

case class Modular(left: Expression, right: Expression) extends BinaryOperation {
	def apply() = left() % right()
	def copy(l: Expression, r: Expression) = Modular(l, r)
}

case class Pow(left: Expression, right: Expression) extends BinaryOperation {
	def apply() = Math.pow(left(), right())
	def copy(l: Expression, r: Expression) = Pow(l, r)
}

case class Or(left: Expression, right: Expression) extends BinaryOperation {
	def apply() = left().toInt | right().toInt
	def copy(l: Expression, r: Expression) = Or(l, r)
}

case class And(left: Expression, right: Expression) extends BinaryOperation {
	def apply() = left().toInt & right().toInt
	def copy(l: Expression, r: Expression) = And(l, r)
}

case class Xor(left: Expression, right: Expression) extends BinaryOperation {
	def apply() = left().toInt & right().toInt
	def copy(l: Expression, r: Expression) = Xor(l, r)
}

case class ShiftLeft(left: Expression, right: Expression) extends BinaryOperation {
	def apply() = left().toInt << right().toInt
	def copy(l: Expression, r: Expression) = ShiftLeft(l, r)
}

case class ShiftRight(left: Expression, right: Expression) extends BinaryOperation {
	def apply() = left().toInt >> right().toInt
	def copy(l: Expression, r: Expression) = ShiftRight(l, r)
}

case class Equals(left: Expression, right: Expression) extends BinaryOperation {
	def apply() = if (left() == right()) 1.0 else 0.0
	def copy(l: Expression, r: Expression) = Equals(l, r)
}

case class NotEquals(left: Expression, right: Expression) extends BinaryOperation {
	def apply() = if (left() != right()) 1.0 else 0.0
	def copy(l: Expression, r: Expression) = NotEquals(l, r)
}

case class GreaterThanOrEquals(left: Expression, right: Expression) extends BinaryOperation {
	def apply() = if (left() >= right()) 1.0 else 0.0
	def copy(l: Expression, r: Expression) = GreaterThanOrEquals(l, r)
}

case class LessThanOrEquals(left: Expression, right: Expression) extends BinaryOperation {
	def apply() = if (left() <= right()) 1.0 else 0.0
	def copy(l: Expression, r: Expression) = LessThanOrEquals(l, r)
}

case class GreaterThan(left: Expression, right: Expression) extends BinaryOperation {
	def apply() = if (left() > right()) 1.0 else 0.0
	def copy(l: Expression, r: Expression) = GreaterThan(l, r)
}

case class LessThan(left: Expression, right: Expression) extends BinaryOperation {
	def apply() = if (left() < right()) 1.0 else 0.0
	def copy(l: Expression, r: Expression) = LessThan(l, r)
}

// vim: set ts=4 sw=4 et:
