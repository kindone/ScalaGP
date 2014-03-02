package kindone.scalagp

trait Expression {
	def apply(implicit table: Vector[Double]): Double
	val size: Int
	def subexpression(pos: Int): Expression
	def copy(): Expression
}

case class Val(val value: Double) extends Expression {
	def apply(implicit table: Vector[Double]) = value
	val size = 1
	def subexpression(pos: Int): Expression = this
	def copy(n: Double) = new Val(n)
	def copy() = copy(value)
	override def toString() = value.toString
}

case class Ref(val id: Int) extends Expression {
	def apply(implicit table: Vector[Double]) = table(Math.abs(id) % table.size)
	val size = 1
	def subexpression(pos: Int): Expression = this
	def copy(n: Int) = new Ref(n)
	def copy() = copy(id)
	override def toString() = s"x_$id"
}

abstract class Operation extends Expression

