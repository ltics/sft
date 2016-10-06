package sft

/**
 * Created by zjh on 16/10/6.
 */
object Syntax {
    type Name = String
    sealed trait Term

    case class Var(name: String) extends Term
    case class Abs(x: String, body: Term) extends Term
    case class App(fun: Term, arg: Term) extends Term
}
