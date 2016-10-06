package sft.stlc

import scala.util.parsing.combinator._
import sft.stlc.Syntax._

/**
 * Created by zjh on 16/10/6.
 */

object STLCParser extends RegexParsers with PackratParsers {

    lazy val int: PackratParser[Type] = {
        "Int" ^^ { _ => TInt }
    }

    lazy val bool: PackratParser[Type] = {
        "Bool" ^^ { _ => TBool }
    }

    lazy val arrow: PackratParser[TArr] = {
        Ty ~ """→""".r ~ Ty ^^ { case t1 ~ _ ~ t2 => TArr(t1, t2) }
    }

    lazy val atomt: PackratParser[Type] = {
        int | bool | "(" ~> Ty <~ ")" ^^ { identity }
    }

    lazy val Ty: PackratParser[Type] = {
        arrow | atomt
    }

    lazy val zero: PackratParser[Term] = {
        "0" ^^ { _ => LZero }
    }

    lazy val t: PackratParser[Term] = {
        "true" ^^ { _ => LTrue }
    }

    lazy val f: PackratParser[Term] = {
        "false" ^^ { _ => LFalse }
    }

    lazy val succ: PackratParser[Succ] = {
        "succ" ~> expression ^^ { Succ }
    }

    lazy val identifier: PackratParser[Var] = {
        """[a-zA-Z]+""".r ^^ { Var }
    }

    lazy val atom: PackratParser[Term] = {
        identifier | zero | t | f | "(" ~> expression <~ ")" ^^ { identity }
    }

    lazy val abstraction: PackratParser[Lam] = {
        """λ""".r ~> """[a-zA-Z]+""".r ~ ":" ~ Ty ~ "." ~ expression ^^ { case i ~ _ ~ ty ~ _ ~ e => Lam(i, ty, e) }
    }

    lazy val application: PackratParser[App] = {
        expression ~ expression ^^ { case e1 ~ e2 => App(e1, e2) }
    }

    lazy val expression: PackratParser[Term] = {
        abstraction | application | succ | atom
    }

    def apply(input: String) = parseAll(expression, input)
}

