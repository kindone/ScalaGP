package app

import kindone.scalagp._
import scala.util.Random

object Application extends App {

	lazy val rand = new Random()

	// prepare initial population
	def init(num: Int): List[ExpressionTree] = {
		(1 to num).foldLeft[List[ExpressionTree]](List()) { (acc, i) =>
			acc :+ ExpressionTree(ExpressionTree.randomTree)
		}
	}

	// fitress evaluation
	def evaluate(individual: ExpressionTree) = {

		var rms = 0.0
		for (i <- 0 until 20) {
			implicit val refs = Vector[Double](rand.nextDouble() * rand.nextInt(10000), rand.nextDouble() * rand.nextInt(10000), rand.nextDouble() * rand.nextInt(10000))
			// run sampled evaluation
			val score_ = individual.evaluate
			val score =
				if (score_.isNaN)
					0.0
				else if (score_.isInfinity)
					0.0
				else
					score_

			val actual = refs(0) + refs(1) + refs(2) * 3 //Math.pow(refs(0), 3) + Math.pow(refs(1), 2) - refs(2)
			rms += Math.pow(score - actual, 2)
		}
		rms
	}

	//routelette wheel selection
	def select(popWithScore: List[(ExpressionTree, Double)]) = {
	}

	// run current generation
	def step(population: List[ExpressionTree]): (List[ExpressionTree], (ExpressionTree, Double)) = {
		// evaluate
		val popWithScore = population.map { individual =>
			val score_ = evaluate(individual)
			val score =
				if (score_.isNaN)
					0.0
				else if (score_.isInfinity)
					Double.MaxValue
				else
					score_
			(individual, score)
		}
		// sort by score
		val sortedPopWithScore = popWithScore.sortWith(_._2 < _._2)
		val best = sortedPopWithScore.head
		// select top 10
		val selected = sortedPopWithScore.take(5)
		// crossover, mutate the top 10
		val reproduced = selected.foldLeft(List[ExpressionTree]()) {
			case (acc, (individual, score)) =>
				val randomIndividual = population(rand.nextInt(population.size))
				val pairs = individual.crossover(randomIndividual)
				acc.+:(pairs._1.mutateLeaf.mutateRoot).+:(pairs._2.mutateLeaf.mutateRoot)
		}
		// replace bottom 10 with the reproduced
		(reproduced ++ sortedPopWithScore.drop(10).map(_._1), best)

	}

	var cur = init(100)
	for (i <- 0 until 1000) {
		val (next, best) = step(cur)
		cur = next
		println(best._1, best._2)
	}

	//val t1 = ExpressionTree(ExpressionTree.randomTree)
	//val t2 = ExpressionTree(ExpressionTree.randomTree)
	//t1.crossover(t2)
	//t1.mutateLeaf
}

