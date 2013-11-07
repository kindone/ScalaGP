package kindone.scalagp


abstract class TernaryOperation
{
	def left:Expression
	def right:Expression
	def center:Expression
	lazy val subtreeSize = left.subtreeSize + center.subtreeSize + right.subtreeSize
}


case class If(left:Expression, center:Expression, right:Expression) extends TernaryOperation {
	def apply() = if(center() > 0.0) left() else right()
}



// vim: set ts=4 sw=4 et:
