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
		for (i <- 0 until 30) {
			implicit val refs = Vector[Double](rand.nextInt(), rand.nextInt(), rand.nextInt())
			// run sampled evaluation
			val estimated = individual.evaluate
			val actual = Math.pow(refs(0), 3) + Math.pow(refs(1), 2) - refs(2)
			rms += Math.pow(estimated - actual, 2)
		}
		rms
	}

	//routelette wheel selection
	def select(popWithScore: List[(ExpressionTree, Double)]) = {
	}

	// run current generation
	def step(population: List[ExpressionTree]): List[ExpressionTree] = {
		// evaluate
		val popWithScore = population.map { individual =>
			(individual, evaluate(individual))
		}
		// sort by score
		val sortedPopWithScore = popWithScore.sortWith(_._2 < _._2)
		// select top 10
		val selected = sortedPopWithScore.take(5)
		// crossover, mutate the top 10
		val reproduced = selected.foldLeft(List[ExpressionTree]()) {
			case (acc, (individual, score)) =>
				val randomIndividual = population(rand.nextInt(population.size))
				val pairs = individual.crossover(randomIndividual)
				acc.+:(pairs._1.mutateLeaf).+:(pairs._2.mutateLeaf)
		}
		// replace bottom 10 with the reproduced
		reproduced ++ sortedPopWithScore.drop(10).map(_._1)
	}

	val initial = init(100)

	val next = step(initial)

	//val t1 = ExpressionTree(ExpressionTree.randomTree)
	//val t2 = ExpressionTree(ExpressionTree.randomTree)
	//t1.crossover(t2)
	//t1.mutateLeaf
}

