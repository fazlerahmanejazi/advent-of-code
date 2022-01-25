package year2021.day6

import utilities.memoize_function

import scala.annotation.tailrec
import scala.language.postfixOps

object lanternfish extends App {

  def calculate(idx: Int, days: Int, fish_count: List[Int]): BigInt = (idx, days) match {
    case (_, 0) => fish_count(idx)
    case (8, _) => calculate(0, days - 1, fish_count)
    case (6, _) => calculate(7, days - 1, fish_count) + calculate(0, days - 1, fish_count)
    case (_, _) => calculate(idx + 1, days - 1, fish_count)
  }

  def initial_count(input: List[Int]): List[Int] = {
    @tailrec def go(i: Int, idx: Int, input: List[Int], acc: Int): Int = {
      if (i >= input.length ) acc
      else if (input(i) == idx) go(i + 1, idx, input, 1 + acc)
      else go(i + 1, idx, input, acc)
    }
    @tailrec def build(i: Int, initial_count: List[Int], input: List[Int]): List[Int] = {
      if (i > 8 ) initial_count
      else build(i + 1, initial_count ::: List(go(0, i, input, 0)), input)
    }
    build(0, List(), input)
  }

  def solve(days: Int, input: List[Int]): BigInt = {
    val calculate_memoized = memoize_function(calculate _ tupled)
    @tailrec def sum(i: Int, days: Int, fish_count: List[Int], acc: BigInt): BigInt = {
      if (i > 8) acc
      else sum(i + 1, days, fish_count, calculate_memoized(i, days, fish_count) + acc)
    }
    sum(0, days, initial_count(input), 0)
  }

  val input = List(2,1,2,1,5,1,5,1,2,2,1,1,5,1,4,4,4,3,1,2,2,3,4,1,1,5,1,1,4,2,5,5,5,1,1,4,5,4,1,1,4,2,1,4,1,2,2,5,1,1,5,1,1,3,4,4,1,2,3,1,5,5,4,1,4,1,2,1,5,1,1,1,3,4,1,1,5,1,5,1,1,5,1,1,4,3,2,4,1,4,1,5,3,3,1,5,1,3,1,1,4,1,4,5,2,3,1,1,1,1,3,1,2,1,5,1,1,5,1,1,1,1,4,1,4,3,1,5,1,1,5,4,4,2,1,4,5,1,1,3,3,1,1,4,2,5,5,2,4,1,4,5,4,5,3,1,4,1,5,2,4,5,3,1,3,2,4,5,4,4,1,5,1,5,1,2,2,1,4,1,1,4,2,2,2,4,1,1,5,3,1,1,5,4,4,1,5,1,3,1,3,2,2,1,1,4,1,4,1,2,2,1,1,3,5,1,2,1,3,1,4,5,1,3,4,1,1,1,1,4,3,3,4,5,1,1,1,1,1,2,4,5,3,4,2,1,1,1,3,3,1,4,1,1,4,2,1,5,1,1,2,3,4,2,5,1,1,1,5,1,1,4,1,2,4,1,1,2,4,3,4,2,3,1,1,2,1,5,4,2,3,5,1,2,3,1,2,2,1,4)
  println(solve(80, input))
}

