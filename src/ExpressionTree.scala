package kindone.scalagp

import scala.util.Random

object ExpressionTree {
	lazy val rand = new Random()

	def randomInt(max: Int) = rand.nextInt(max)
	def randomDouble() = rand.nextDouble()

	def randomTerminal(): Expression = {
		val n = randomInt(2)
		n match {
			case 0 => Val(randomDouble * randomInt(10000))
			case 1 => Val(randomDouble)
			case _ => throw new RuntimeException("Unexpected random number")
		}
	}

	def randomTree(): Expression = {
		val n = randomInt(20)
		n match {
			case 0 => Val(randomDouble * randomInt(10000))
			case 1 => Val(randomDouble) //Ref(randomInt(1000), Vector(1)) // FIXME
			case 2 => If(randomTerminal, randomTerminal, randomTerminal)
			case 3 => Add(randomTerminal, randomTerminal)
			case 4 => Subtract(randomTerminal, randomTerminal)
			case 5 => Multiply(randomTerminal, randomTerminal)
			case 6 => Divide(randomTerminal, randomTerminal)
			case 7 => Modular(randomTerminal, randomTerminal)
			case 8 => Pow(randomTerminal, randomTerminal)
			case 9 => Or(randomTerminal, randomTerminal)
			case 10 => And(randomTerminal, randomTerminal)
			case 11 => Xor(randomTerminal, randomTerminal)
			case 12 => ShiftLeft(randomTerminal, randomTerminal)
			case 13 => ShiftRight(randomTerminal, randomTerminal)
			case 14 => Equals(randomTerminal, randomTerminal)
			case 15 => NotEquals(randomTerminal, randomTerminal)
			case 16 => GreaterThanOrEquals(randomTerminal, randomTerminal)
			case 17 => LessThanOrEquals(randomTerminal, randomTerminal)
			case 18 => GreaterThan(randomTerminal, randomTerminal)
			case 19 => LessThan(randomTerminal, randomTerminal)
			case _ => throw new RuntimeException("Unexpected random number")
		}
	}
}

case class ExpressionTree(val root: Expression) {
	def evaluate() = root()

	// construct by replacing oldsub with newsub
	def replace(oldsub: Expression, newsub: Expression) = {
		if (oldsub eq root)
			throw new RuntimeException("unexpected self-referencing subexpression")

		// traverse down the tree, and build the subtree
		def build(sub: Expression): Expression = {
			sub match {
				case v: Val => v
				case r: Ref => r
				case u: UnaryOperation =>
					if (u.center eq oldsub)
						u.copy(newsub)
					else {
						val c = build(u.center)
						if (c eq u.center)
							u
						else
							u.copy(build(u.center))
					}
				case b: BinaryOperation =>
					if (b.left eq oldsub)
						b.copy(newsub, b.right)
					else if (b.right eq oldsub)
						b.copy(b.left, newsub)
					else {
						val (l, r): (Expression, Expression) = (build(b.left), build(b.right))
						if ((l eq b.left) && (r eq b.right))
							b
						else
							b.copy(l, r)
					}
				case t: TernaryOperation =>
					if (t.left eq oldsub)
						t.copy(newsub, t.center, t.right)
					else if (t.center eq oldsub)
						t.copy(t.left, newsub, t.right)
					else if (t.right eq oldsub)
						t.copy(t.left, t.center, newsub)
					else {
						val (l, c, r): (Expression, Expression, Expression) = (build(t.left), build(t.center), build(t.right))
						if ((l eq t.left) && (c eq t.center) && (r eq t.right))
							t
						else
							t.copy(l, c, r)
					}
			}
		}

		build(root)
	}

	// for crossover
	// given two expression trees, generate another pair of expression trees
	def crossover(other: ExpressionTree): (ExpressionTree, ExpressionTree) = {
		val a = ExpressionTree.randomInt(root.size - 1)
		val b = ExpressionTree.randomInt(other.root.size - 1)

		val subtree1 = root.subexpression(a)
		val subtree2 = other.root.subexpression(b)

		val newtree1 = replace(subtree1, subtree2)
		val newtree2 = other.replace(subtree2, subtree1)

		(ExpressionTree(newtree1), ExpressionTree(newtree2))
	}

	// 
	def mutateLeaf() = {
		val a = ExpressionTree.randomInt(root.size - 1)
		val target = root.subexpression(a)

		replace(target, ExpressionTree.randomTree)
	}

}
