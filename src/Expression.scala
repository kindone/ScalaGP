package kindone.scalagp

trait Branch1
{
	self: {def center:Expression } => 
	lazy val subtreeSize = center.subtreeSize
}

trait Branch2
{
	self: {def left:Expression; def right:Expression } => 
	lazy val subtreeSize = left.subtreeSize + right.subtreeSize
}

trait Branch3
{
	self: {def left:Expression; def center:Expression; def right:Expression } => 
	lazy val subtreeSize = left.subtreeSize + center.subtreeSize + right.subtreeSize
}


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
{
}

case class Add(left:Expression, right:Expression) extends Operation with Branch2 {
	def apply() = left() + right()
}

case class Subtract(left:Expression, right:Expression) extends Operation with Branch2 {
	def apply() = left() - right()
} 

case class Multiply(left:Expression, right:Expression) extends Operation with Branch2 {
	def apply() = left() * right()
}

case class Divide(left:Expression, right:Expression) extends Operation with Branch2 {
	def apply() = if(scala.math.abs(right()) <= 0.1E-6) 0.0 else left() / right()
}

case class Modular(left:Expression, right:Expression) extends Operation with Branch2 {
	def apply() = left() % right()
}

case class Pow(left:Expression, right:Expression) extends Operation with Branch2 {
	def apply() = Math.pow(left(), right())
}

case class Sqrt(center:Expression) extends Operation with Branch1 {
	def apply() = Math.sqrt(center())
}

case class Ceil(center:Expression) extends Operation with Branch1 {
	def apply() = Math.ceil(center())
}

case class Floor(center:Expression) extends Operation with Branch1 {
	def apply() = Math.floor(center())
}

case class ToInt(left:Expression, right:Expression) extends Operation with Branch2 {
	def apply() = left().toInt
}

case class ToBoolean(center:Expression) extends Operation with Branch1 {
	def apply() = if(center() > 0.0) 1.0 else 0.0
}


case class Or(left:Expression, right:Expression) extends Operation with Branch2 {
	def apply() = left().toInt | right().toInt
}

case class And(left:Expression, right:Expression) extends Operation with Branch2 {
	def apply() = left().toInt & right().toInt
}

case class Not(center:Expression) extends Operation with Branch1 {
	def apply() = if(center() > 0.0) 0.0 else 1.0
}

case class Xor(left:Expression, right:Expression) extends Operation with Branch2 {
	def apply() = left().toInt & right().toInt
}

case class ShiftLeft(left:Expression, right:Expression) extends Operation with Branch2 {
	def apply() = left().toInt << right().toInt
}

case class ShiftRight(left:Expression, right:Expression) extends Operation with Branch2 {
	def apply() = left().toInt >> right().toInt
}

case class Equals(left:Expression, right:Expression) extends Operation with Branch2 {
	def apply() = if(left() == right()) 1.0 else 0.0
}

case class NotEquals(left:Expression, right:Expression) extends Operation with Branch2 {
	def apply() = if(left() != right()) 1.0 else 0.0
}

case class GreaterThanOrEquals(left:Expression, right:Expression) extends Operation with Branch2 {
	def apply() = if(left() >= right()) 1.0 else 0.0
}

case class LessThanOrEquals(left:Expression, right:Expression) extends Operation with Branch2 {
	def apply() = if(left() <= right()) 1.0 else 0.0
}

case class GreaterThan(left:Expression, right:Expression) extends Operation with Branch2 {
	def apply() = if(left() > right()) 1.0 else 0.0
}

case class LessThan(left:Expression, right:Expression) extends Operation with Branch2 {
	def apply() = if(left() < right()) 1.0 else 0.0
}

case class IsZero(center:Expression) extends Operation with Branch1 {
	def apply() = if(center() == 0.0) 1.0 else 0.0
}

case class IsNonZero(center:Expression) extends Operation with Branch1 {
	def apply() = if(center() != 0.0) 1.0 else 0.0
}

case class If(left:Expression, center:Expression, right:Expression) extends Operation with Branch3 {
	def apply() = if(center() > 0.0) left() else right()
}

class ExpressionTree(val root:Expression)
{
	def evaluate() = root()

	// for crossover
//	def splice():(ExpressionTree, ExpressionTree) = {}

	def mutate() = {}

}
