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

	def copy(newCenter: Expression): Expression
}

case class Sqrt(center: Expression) extends UnaryOperation {
	def apply(implicit table: Vector[Double]) = Math.sqrt(center.apply)
	def copy(c: Expression) = Sqrt(c)
}

case class Ceil(center: Expression) extends UnaryOperation {
	def apply(implicit table: Vector[Double]) = Math.ceil(center.apply)
	def copy(c: Expression) = Ceil(c)
}

case class Floor(center: Expression) extends UnaryOperation {
	def apply(implicit table: Vector[Double]) = Math.floor(center.apply)
	def copy(c: Expression) = Floor(c)
}

case class ToInt(center: Expression) extends UnaryOperation {
	def apply(implicit table: Vector[Double]) = center.apply.toInt
	def copy(c: Expression) = ToInt(c)
}

case class ToBoolean(center: Expression) extends UnaryOperation {
	def apply(implicit table: Vector[Double]) = if (center.apply > 0.0) 1.0 else 0.0
	def copy(c: Expression) = ToBoolean(c)
}

case class Not(center: Expression) extends UnaryOperation {
	def apply(implicit table: Vector[Double]) = if (center.apply > 0.0) 0.0 else 1.0
	def copy(c: Expression) = Not(c)
}

case class IsZero(center: Expression) extends UnaryOperation {
	def apply(implicit table: Vector[Double]) = if (center.apply == 0.0) 1.0 else 0.0
	def copy(c: Expression) = IsZero(c)
}

case class IsNonZero(center: Expression) extends UnaryOperation {
	def apply(implicit table: Vector[Double]) = if (center.apply != 0.0) 1.0 else 0.0
	def copy(c: Expression) = IsNonZero(c)
}

