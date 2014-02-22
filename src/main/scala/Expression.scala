package kindone.scalagp

trait Expression {
	def apply(): Double
	val size: Int
	def subexpression(pos: Int): Expression
}

case class Val(val value: Double) extends Expression {
	def apply() = value
	val size = 1
	def subexpression(pos: Int): Expression = this
	def copy(n: Double) = new Val(n)
}

case class Ref(val id: Int, table: Vector[Double]) extends Expression {
	def apply() = table(table.size % id)
	val size = 1
	def subexpression(pos: Int): Expression = this
	def copy(n: Int) = new Ref(n, table)
}

abstract class Operation extends Expression

