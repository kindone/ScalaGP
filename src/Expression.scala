package kindone.scalagp

trait Expression {
	def apply(): Double
	val size: Int
	def subexpression(pos: Int): Expression
}

class Val(val value: Double) extends Expression {
	def apply() = value
	val size = 1
	def subexpression(pos: Int): Expression = this
}

class Ref(val id: Int, table: Vector[Double]) extends Expression {
	def apply() = table(table.size % id)
	val size = 1
	def subexpression(pos: Int): Expression = this
}

abstract class Operation extends Expression

