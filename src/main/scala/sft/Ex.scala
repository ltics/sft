package sft

import sft.Syntax._

/**
 * Created by zjh on 16/10/6.
 */

object Ex {
    def removeOdd(l: List[Int]): List[Int] = l.filter(_ % 2 == 0)

    def fact: Int => Int = {
        case 0 => 1
        case n => n * fact(n - 1)
    }

    def factList(l: List[Int]): List[Int] = l.map(fact)

    def qsort: List[Int] => List[Int] = {
        case Nil     => Nil
        case x :: xs => {
            val small = for {
                y <- xs
                if y <= x // guard in Haskell do notation
            } yield y

            val large = for {
                y <- xs
                if y > x
            } yield y

            qsort(small) ++ (x :: qsort(large))
        }
    }

    val idExpr = Abs("x", Var("x")) // λx. x
    val constExpr = Abs("y", Abs("x", Var("y"))) // λy. λx. y
    // (λx. x x) (λx. x x)
    val fixpointExpr = {
        val selfExpr = Abs("x", App(Var("x"), Var("x")))
        App(selfExpr, selfExpr)
    }

    // Add checkdiff method to the List class to check that all elements of a list are different
    implicit class ListExtra[A](l: List[A]) {
        def checkdiff: Boolean = l match {
            case Nil    => true
            case h :: t => {
                !t.contains(h) && t.checkdiff
            }
        }
    }

    sealed trait EC // electrical circuits
    case class Resistor(resistance: Double) extends EC
    case class Series(ec1: EC, ec2: EC) extends EC
    case class Parallel(ec1: EC, ec2: EC) extends EC

    def ohmsLaw: EC => Double = {
        case Resistor(r)        => r
        case Series(ec1, ec2)   => ohmsLaw(ec1) + ohmsLaw(ec2)
        case Parallel(ec1, ec2) => 1 / (1 / ohmsLaw(ec1) + 1 / ohmsLaw(ec2))
    }
}
