package kindone.scalagp

trait Expression {
	def apply(implicit table: Vector[Double]): Double
	val size: Int
	def subexpression(pos: Int): Expression
}

case class Val(val value: Double) extends Expression {
	def apply(implicit table: Vector[Double]) = value
	val size = 1
	def subexpression(pos: Int): Expression = this
	def copy(n: Double) = new Val(n)
}

case class Ref(val id: Int) extends Expression {
	def apply(implicit table: Vector[Double]) = table(table.size % id)
	val size = 1
	def subexpression(pos: Int): Expression = this
	def copy(n: Int) = new Ref(n)
}

abstract class Operation extends Expression

