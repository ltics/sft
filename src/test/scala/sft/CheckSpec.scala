package sft

import org.scalatest.{Matchers, FlatSpec}
import checker.Syntax._
import checker.Checker._
import sft.checker.{TypeFailure, TypeSuccess}

/**
 * Created by zjh on 16/10/6.
 */

class CheckSpec extends FlatSpec with Matchers {
    it should "check type" in {
        check(Var("x")) should be (TypeFailure("unbound name: x"))
        check(Lam("x", TInt, Var("y"))) should be (TypeFailure("unbound name: y"))
        check(Lam("x", TInt, Var("x"))) should be (TypeSuccess(TArr(TInt, TInt)))
        check(App(Lam("x", TInt, Var("x")), LZero)) should be (TypeSuccess(TInt))
        check(App(Lam("x", TArr(TInt, TInt), App(Var("x"), Succ(LZero))), Lam("x", TInt, Succ(Var("x"))))) should be (TypeSuccess(TInt))
    }
}
