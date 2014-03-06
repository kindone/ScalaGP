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
	def copy() = copy(left.copy, right.copy)

}

case class Add(left: Expression, right: Expression) extends BinaryOperation {
	def apply(implicit table: Vector[Double]) = left.apply + right.apply
	def copy(l: Expression, r: Expression) = Add(l, r)
	override def toString() = s"($left) + ($right)"
}

case class Subtract(left: Expression, right: Expression) extends BinaryOperation {
	def apply(implicit table: Vector[Double]) = left.apply - right.apply
	def copy(l: Expression, r: Expression) = Subtract(l, r)
	override def toString() = s"($left) - ($right)"
}

case class Multiply(left: Expression, right: Expression) extends BinaryOperation {
	def apply(implicit table: Vector[Double]) = left.apply * right.apply
	def copy(l: Expression, r: Expression) = Multiply(l, r)
	override def toString() = s"($left) * ($right)"
}

case class Divide(left: Expression, right: Expression) extends BinaryOperation {
	def apply(implicit table: Vector[Double]) = if (scala.math.abs(right.apply) <= 0.1E-6) 0.0 else left.apply / right.apply
	def copy(l: Expression, r: Expression) = Divide(l, r)
	override def toString() = s"($left) / ($right)"
}

case class Modular(left: Expression, right: Expression) extends BinaryOperation {
	def apply(implicit table: Vector[Double]) = left.apply % right.apply
	def copy(l: Expression, r: Expression) = Modular(l, r)
	override def toString() = s"($left) % ($right)"
}

case class Pow(left: Expression, right: Expression) extends BinaryOperation {
	def apply(implicit table: Vector[Double]) = Math.pow(left.apply, right.apply)
	def copy(l: Expression, r: Expression) = Pow(l, r)
	override def toString() = s"($left) ** ($right)"
}

case class Or(left: Expression, right: Expression) extends BinaryOperation {
	def apply(implicit table: Vector[Double]) = left.apply.toInt | right.apply.toInt
	def copy(l: Expression, r: Expression) = Or(l, r)
	override def toString() = s"($left) | ($right)"
}

case class And(left: Expression, right: Expression) extends BinaryOperation {
	def apply(implicit table: Vector[Double]) = left.apply.toInt & right.apply.toInt
	def copy(l: Expression, r: Expression) = And(l, r)
	override def toString() = s"($left) & ($right)"
}

case class Xor(left: Expression, right: Expression) extends BinaryOperation {
	def apply(implicit table: Vector[Double]) = left.apply.toInt & right.apply.toInt
	def copy(l: Expression, r: Expression) = Xor(l, r)
	override def toString() = s"($left) ^ ($right)"
}

case class ShiftLeft(left: Expression, right: Expression) extends BinaryOperation {
	def apply(implicit table: Vector[Double]) = left.apply.toInt << right.apply.toInt
	def copy(l: Expression, r: Expression) = ShiftLeft(l, r)
	override def toString() = s"($left) << ($right)"
}

case class ShiftRight(left: Expression, right: Expression) extends BinaryOperation {
	def apply(implicit table: Vector[Double]) = left.apply.toInt >> right.apply.toInt
	def copy(l: Expression, r: Expression) = ShiftRight(l, r)
	override def toString() = s"($left) >> ($right)"
}

case class Equals(left: Expression, right: Expression) extends BinaryOperation {
	def apply(implicit table: Vector[Double]) = if (left.apply == right.apply) 1.0 else 0.0
	def copy(l: Expression, r: Expression) = Equals(l, r)
	override def toString() = s"($left) == ($right)"
}

case class NotEquals(left: Expression, right: Expression) extends BinaryOperation {
	def apply(implicit table: Vector[Double]) = if (left.apply != right.apply) 1.0 else 0.0
	def copy(l: Expression, r: Expression) = NotEquals(l, r)
	override def toString() = s"($left) != ($right)"
}

case class GreaterThanOrEquals(left: Expression, right: Expression) extends BinaryOperation {
	def apply(implicit table: Vector[Double]) = if (left.apply >= right.apply) 1.0 else 0.0
	def copy(l: Expression, r: Expression) = GreaterThanOrEquals(l, r)
	override def toString() = s"($left) >= ($right)"
}

case class LessThanOrEquals(left: Expression, right: Expression) extends BinaryOperation {
	def apply(implicit table: Vector[Double]) = if (left.apply <= right.apply) 1.0 else 0.0
	def copy(l: Expression, r: Expression) = LessThanOrEquals(l, r)
	override def toString() = s"($left) <= ($right)"
}

case class GreaterThan(left: Expression, right: Expression) extends BinaryOperation {
	def apply(implicit table: Vector[Double]) = if (left.apply > right.apply) 1.0 else 0.0
	def copy(l: Expression, r: Expression) = GreaterThan(l, r)
	override def toString() = s"($left) > ($right)"
}

case class LessThan(left: Expression, right: Expression) extends BinaryOperation {
	def apply(implicit table: Vector[Double]) = if (left.apply < right.apply) 1.0 else 0.0
	def copy(l: Expression, r: Expression) = LessThan(l, r)
	override def toString() = s"($left) < ($right)"
}

// vim: set ts=4 sw=4 et:
