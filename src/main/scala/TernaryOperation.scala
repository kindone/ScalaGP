package kindone.scalagp

abstract class TernaryOperation extends Operation {
	def left: Expression
	def right: Expression
	def center: Expression
	lazy val size = left.size + center.size + right.size + 1
	def subexpression(pos: Int): Expression = {
		if (pos >= size - 1)
			this
		else if (pos == size - 2)
			right
		else if (pos >= center.size + left.size)
			right.subexpression(pos - center.size - left.size)
		else if (pos == center.size + left.size - 1)
			center
		else if (pos >= left.size)
			center.subexpression(pos - left.size)
		else if (pos == left.size - 1)
			left
		else
			left.subexpression(pos)
	}

	def copy(l: Expression, c: Expression, r: Expression): Expression
}

case class If(left: Expression, center: Expression, right: Expression) extends TernaryOperation {
	def apply() = if (center() > 0.0) left() else right()
	def copy(l: Expression, c: Expression, r: Expression) = If(l, c, r)
}

// vim: set ts=4 sw=4 et:
