package app

import kindone.scalagp._
import scala.util.Random

object Application extends App {

	lazy val rand = new Random()

	// prepare initial population
	def init(num: Int): List[ExpressionTree] = {
		(0 until num).foldLeft[List[ExpressionTree]](List()) { (acc, i) =>
			acc :+ ExpressionTree(ExpressionTree.randomTree)
		}
	}

	def samples(num: Int) = {
		(0 until num).foldLeft[Seq[Seq[Double]]](Seq()) { (acc, i) =>
			acc.+:(Seq[Double](rand.nextDouble() * rand.nextInt(100000), rand.nextDouble() * rand.nextInt(100000), rand.nextDouble() * rand.nextInt(100000)))
		}
	}

	// fitress evaluation
	def evaluate(individual: ExpressionTree, listOfSamples: Seq[Seq[Double]]) = {

		listOfSamples.foldLeft[Double](0.0) { (acc, samples) =>
			// run sampled evaluation
			implicit val refs: Vector[Double] = samples.toVector
			val actual = Math.pow(refs(0), 3) + Math.pow(refs(1), 2) - refs(2)
			val score_ = individual.evaluate
			val score =
				if (score_.isNaN)
					0.0
				else if (score_.isInfinity)
					Double.MaxValue
				else
					score_
			acc + Math.pow(score - actual, 2)
		} + Math.pow(2, individual.root.size)*1.0E-7
	}

	//routelette wheel selection
	def select(popWithScore: List[(ExpressionTree, Double)]) = {
	}

	// run current generation
	def step(population: List[ExpressionTree]): (List[ExpressionTree], (ExpressionTree, Double)) = {
		// evaluate
		val sampleSet = samples(100)
		val popWithScore = population.map { individual =>
			val score_ = evaluate(individual, sampleSet)
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
				val mutatedPairs1 = List(pairs._1, pairs._2).map { individual =>
					if (rand.nextFloat < 0.1)
						individual.mutateRoot
					else
						individual
				}
				val mutatedPairs2 = mutatedPairs1.map { individual =>
					if (rand.nextFloat < 0.1)
						individual.mutateLeaf
					else
						individual
				}
				acc.+:(mutatedPairs2.head).+:(mutatedPairs2.last)
		}
		// replace bottom 10 with the reproduced
		(reproduced ++ sortedPopWithScore.dropRight(10).map(_._1), best)

	}

	var cur = init(100)
	var bestSoFar = Double.MaxValue
	println(bestSoFar)
	for (i <- 0 until 1000) {
		val (next, best) = step(cur)
		cur = next
		if (bestSoFar > best._2) {
			bestSoFar = best._2
			println(best._1, best._2)
		}
	}

	//val t1 = ExpressionTree(ExpressionTree.randomTree)
	//val t2 = ExpressionTree(ExpressionTree.randomTree)
	//t1.crossover(t2)
	//t1.mutateLeaf
}

