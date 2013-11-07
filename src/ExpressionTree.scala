package kindone.scalagp


class ExpressionTree(val root:Expression)
{
	def evaluate() = root()

	// for crossover
//	def splice():(ExpressionTree, ExpressionTree) = {}

	def mutate() = {}

}
