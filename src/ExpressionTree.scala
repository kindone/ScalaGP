package kindone.scalagp

class ExpressionTree(val root: Expression) {
	def evaluate() = root()

	// for crossover
	// given two expression trees, generate another pair of expression trees
	def splice(other: ExpressionTree): (ExpressionTree, ExpressionTree) = {
		def subtree(tree: ExpressionTree) = {
			tree.root
		}
		(this, other)
	}

	// 
	def mutateLeaf() = {}

}
