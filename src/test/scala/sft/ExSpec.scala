package sft

import org.scalatest.{Matchers, FlatSpec}
import ex.Ex._

/**
 * Created by zjh on 16/10/6.
 */
class ExSpec extends FlatSpec with Matchers {
    it should "remove odd numbers form a list of numbers" in {
        removeOdd((1 to 6).toList) should be ((2 to 6 by 2).toList)
    }

    it should "compute a list of factorials" in {
        factList((1 to 5).toList) should be (List(1, 2, 6, 24, 120))
    }

    it should "sort a list of numbers using quick sort algorithm" in {
        qsort(List(2, 1, 6, 3, 4, 5)) should be ((1 to 6).toList)
    }

    it should "check all elements of a list are different" in {
        List.fill(3)(1).checkdiff should be (right = false)
        List((1 to 6).toList).checkdiff should be (right = true)
        List(1,2,2).checkdiff should be (right = false)
    }

    it should "compute resistance of electrical circuits by Ohm’s law" in {
        val ec = Series(Resistor(2.0), Parallel(Series(Resistor(1.0), Resistor(1.0)), Resistor(2.0)))
        ohmsLaw(ec) should be (3.0)
    }
}
