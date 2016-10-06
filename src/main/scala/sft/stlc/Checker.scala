package sft.stlc

import Syntax._

/**
 * Created by zjh on 16/10/6.
 */

sealed abstract class TypeResult[+T] {
    def map[U](f: T => U): TypeResult[U]

    def flatMap[U](f: T => TypeResult[U]): TypeResult[U]

    def filter(f: T => Boolean): TypeResult[T]
}

case class TypeSuccess[+T](result: T) extends TypeResult[T] {
    override def map[U](f: (T) => U): TypeResult[U] = TypeSuccess(f(this.result))

    override def flatMap[U](f: (T) => TypeResult[U]): TypeResult[U] = f(this.result)

    def filter(f: T => Boolean): TypeResult[T] = f(this.result) match {
        case true  => this
        case false => TypeFailure("do not match")
    }
}

case class TypeFailure(cause: String) extends TypeResult[Nothing] {
    override def map[U](f: (Nothing) => U): TypeResult[U] = this

    override def flatMap[U](f: (Nothing) => TypeResult[U]): TypeResult[U] = this

    override def filter(f: (Nothing) => Boolean): TypeResult[Nothing] = this
}

abstract class Typer {
    def apply(n: Name): TypeResult[Type]

    def newTyper(f: Name => TypeResult[Type]): Typer =
        new Typer {
            def apply(n: Name) = f(n)
        }

    def update(n: Name, tp: Type): Typer = newTyper({ name => if (name == n) TypeSuccess(tp) else apply(name) })

    def typeOf(tm: Term): TypeResult[Type] = tm match {
        case Var(n)          => this(n) // apply(n) here actually
        case Lam(x, from, b) => for {
            to <- update(x, from).typeOf(b)
        } yield TArr(from, to)
        case App(fun, arg)   => for {
            funTp <- typeOf(fun)
            if isTArr(funTp)
            TArr(from, to) = funTp
            tpArg <- typeOf(arg)
            if isEqual(tpArg, from)
        } yield to
        case LTrue           => TypeSuccess(TBool)
        case LFalse          => TypeSuccess(TBool)
        case LZero           => TypeSuccess(TInt)
        case Succ(num)       => {
            this.typeOf(num) match {
                case TypeSuccess(TInt) => TypeSuccess(TInt)
                case _             => TypeFailure("argument of succ is not a number")
            }
        }
    }
}

object Checker {
    def empty: Typer = new Typer {
        override def apply(n: Name): TypeResult[Type] = TypeFailure("unbound name: " + n)
    }

    def check(tm: Term): TypeResult[Type] = empty.typeOf(tm)
}
