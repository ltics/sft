package sft.stlc

/**
 * Created by zjh on 16/10/6.
 */

object Syntax {
    type Name = String
    sealed trait Term

    case class Var(name: Name) extends Term
    case class Lam(x: Name, t: Type, b: Term) extends Term
    case class App(fun: Term, arg: Term) extends Term
    case object LTrue extends Term
    case object LFalse extends Term
    case object LZero extends Term
    case class Succ(num: Term) extends Term

    sealed trait Type
    case class TArr(from: Type, to: Type) extends Type
    case object TBool extends Type
    case object TInt extends Type

    def isTArr: Type => Boolean = {
        case TArr(_, _) => true
        case _          => false
    }

    def isEqual(t1: Type, t2: Type): Boolean = (t1, t2) match {
        case (TArr(_, _), TArr(_, _)) => true
        case (TBool, TBool)           => true
        case (TInt, TInt)             => true
        case _                        => false
    }
}
