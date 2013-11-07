package kindone.scalagp

abstract class UnaryOperation
{
	def center:Expression
	lazy val subtreeSize = center.subtreeSize
}

case class Sqrt(center:Expression) extends UnaryOperation {
	def apply() = Math.sqrt(center())
}

case class Ceil(center:Expression) extends UnaryOperation {
	def apply() = Math.ceil(center())
}

case class Floor(center:Expression) extends UnaryOperation {
	def apply() = Math.floor(center())
}

case class ToInt(left:Expression, right:Expression) extends BinaryOperation {
	def apply() = left().toInt
}

case class ToBoolean(center:Expression) extends UnaryOperation {
	def apply() = if(center() > 0.0) 1.0 else 0.0
}

case class Not(center:Expression) extends UnaryOperation {
	def apply() = if(center() > 0.0) 0.0 else 1.0
}

case class IsZero(center:Expression) extends UnaryOperation {
	def apply() = if(center() == 0.0) 1.0 else 0.0
}

case class IsNonZero(center:Expression) extends UnaryOperation {
	def apply() = if(center() != 0.0) 1.0 else 0.0
}

