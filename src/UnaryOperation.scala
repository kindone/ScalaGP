package kindone.scalagp

abstract class UnaryOperation extends Operation {
	def center: Expression
	lazy val size = center.size + 1
	def subexpression(pos: Int): Expression = {
		if (pos >= size - 1)
			this
		else if (pos == size - 2)
			center
		else
			center.subexpression(pos)
	}
}

case class Sqrt(center: Expression) extends UnaryOperation {
	def apply() = Math.sqrt(center())
}

case class Ceil(center: Expression) extends UnaryOperation {
	def apply() = Math.ceil(center())
}

case class Floor(center: Expression) extends UnaryOperation {
	def apply() = Math.floor(center())
}

case class ToInt(left: Expression, right: Expression) extends BinaryOperation {
	def apply() = left().toInt
}

case class ToBoolean(center: Expression) extends UnaryOperation {
	def apply() = if (center() > 0.0) 1.0 else 0.0
}

case class Not(center: Expression) extends UnaryOperation {
	def apply() = if (center() > 0.0) 0.0 else 1.0
}

case class IsZero(center: Expression) extends UnaryOperation {
	def apply() = if (center() == 0.0) 1.0 else 0.0
}

case class IsNonZero(center: Expression) extends UnaryOperation {
	def apply() = if (center() != 0.0) 1.0 else 0.0
}

