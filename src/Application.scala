package app

import kindone.scalagp._

object Application extends App {

	def init(num: Int): List[ExpressionTree] = {
		(1 to num).foldLeft[List[ExpressionTree]](List()) { (acc, i) =>
			acc :+ ExpressionTree(ExpressionTree.randomTree)
		}
	}

	def evaluate(individual: ExpressionTree) = {
		// run fitness evaluation
		0.0
	}

	//routelette wheel selection
	def select() = {

	}

	def step(population: List[ExpressionTree]): List[ExpressionTree] = {
		// evaluate
		val popWithScore = population.map { individual =>
			(individual, evaluate(individual))
		}
		// select 
		val sortedPopWithScore = popWithScore.sortWith(_._2 > _._2)
		val selected = sortedPopWithScore.take(10)
		// crossover, mutate
		val reproduced = selected.map { (individual, score) =>
			individual.crossover
		}
		// replace
		val replaced = popWithScore.drop(10)

		population
	}

	val initial = init(100)

	val t1 = ExpressionTree(ExpressionTree.randomTree)
	val t2 = ExpressionTree(ExpressionTree.randomTree)
	t1.crossover(t2)
	t1.mutateLeaf
}

