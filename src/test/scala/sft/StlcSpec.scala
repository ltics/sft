package sft

import org.scalatest.{Matchers, FlatSpec}
import stlc.Syntax._
import stlc.Checker._
import sft.stlc.{STLCParser, TypeFailure, TypeSuccess}

/**
 * Created by zjh on 16/10/6.
 */

class STLCSpec extends FlatSpec with Matchers {
    it should "check type" in {
        check(Var("x")) should be(TypeFailure("unbound name: x"))
        check(Lam("x", TInt, Var("y"))) should be(TypeFailure("unbound name: y"))
        check(Lam("x", TInt, Var("x"))) should be(TypeSuccess(TArr(TInt, TInt)))
        check(App(Lam("x", TInt, Var("x")), LZero)) should be(TypeSuccess(TInt))
        check(App(Lam("x", TArr(TInt, TInt), App(Var("x"), Succ(LZero))), Lam("x", TInt, Succ(Var("x"))))) should be(TypeSuccess(TInt))
    }

    it should "parse expression" in {
        STLCParser("λ x : Int. x").get should be (Lam("x", TInt, Var("x")))
        STLCParser("(λ x : Int. x) (succ succ succ 0)").get should be (App(Lam("x", TInt, Var("x")), Succ(Succ(Succ(LZero)))))
        STLCParser("λ x : Int → Bool. x 0").get should be (Lam("x", TArr(TInt, TBool), App(Var("x"), LZero)))
    }
}
