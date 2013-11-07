package kindone.scalagp


trait Expression
{
	def apply():Double
	val subtreeSize:Int
}

class Val(val value:Double) extends Expression
{
	def apply() = value
	val subtreeSize = 1
}

class Ref(val id:Int, table:Vector[Double]) extends Expression
{
	def apply() = table(table.size % id)
	val subtreeSize = 1
}

abstract class Operation extends Expression


