package kindone.scalagp

import scala.util.Random

object ExpressionTree {
	lazy val rand = new Random()

	def nextInt(max: Int) = rand.nextInt(max)
}

class ExpressionTree(val root: Expression) {
	def evaluate() = root()

	// construct by replacing oldsub with newsub
	def replace(oldsub: Expression, newsub: Expression) = {
		if (oldsub eq root)
			throw new RuntimeException("unexpected self-referencing subexpression")

		// traverse down the tree, and build the subtree
		def build(sub: Expression, cur: Expression) = {

		}

		build(root, oldsub)

		this
	}

	// for crossover
	// given two expression trees, generate another pair of expression trees
	def splice(other: ExpressionTree): (ExpressionTree, ExpressionTree) = {
		val a = ExpressionTree.nextInt(root.size - 1)
		val b = ExpressionTree.nextInt(other.root.size - 1)

		val subtree1 = root.subexpression(a)
		val subtree2 = other.root.subexpression(b)

		val newtree1 = replace(subtree1, subtree2)
		val newtree2 = other.replace(subtree2, subtree1)

		(newtree1, newtree2)
	}

	// 
	def mutateLeaf() = {}

}
