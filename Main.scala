import Util._ 

object Main extends Homework08 {
    trait KXCFAEValue
    case class NumV(n: Int) extends KXCFAEValue 
    case class CloV(params: List[String], body: KXCFAE, env: Map[String, KXCFAEValue]) extends KXCFAEValue 
    case class ContV(proc: Cont) extends KXCFAEValue
    case class ThrowV() extends KXCFAEValue
    type Cont = KXCFAEValue => KXCFAEValue

    def run(str: String): String = {
        val epr = KXCFAE.apply(str)
        val defaultEnv = Map[String, KXCFAEValue]()
        val k: Cont = v => v
        def eval(epr : KXCFAE, env: Map[String, KXCFAEValue], k: Cont): KXCFAEValue = {   

            def add(a: KXCFAEValue, b: KXCFAEValue) : KXCFAEValue = (a, b) match {
                case (NumV(numA), NumV(numB)) => NumV(numA + numB)
                case _ => error("can not add FAWEValue") 
            }
            def sub(a: KXCFAEValue, b: KXCFAEValue) : KXCFAEValue = (a, b) match {
                case (NumV(numA), NumV(numB)) => NumV(numA - numB)
                case _ => error("can not add FAWEValue") 
            }
            epr match {
                case Num(n) => k(NumV(n))
                case Add(l, r) => eval(l, env, lv => eval(r, env, rv=>k(add(lv, rv))))
                case Sub(l, r) => eval(l, env, lv => eval(r, env, rv=>k(sub(lv, rv))))
                case Id(id) => k(env.getOrElse(id, error(s"free identifier; $id")))
                case Fun(args, body) => k(CloV(args, body, env))
                case App(func, args) => eval(func, env, _ match {
                    case CloV(params, body, fenv) => {
                        if (params.length != args.length)    error("wrong arity")
                        if (args.length == 0) {
                            eval(body, fenv, k)
                        }
                        else {
                            val paramEnv = Map[String, KXCFAEValue]()
                            def recCont(remainArgs: Int, paramEnv: Map[String, KXCFAEValue]): KXCFAEValue => KXCFAEValue = {
                                val headArg = args.length - remainArgs
                                remainArgs match {
                                    case 0 =>
                                        argV => eval(body, fenv ++ (paramEnv + (params(headArg - 1) -> argV)), k)
                                    case _ =>
                                        argV => eval(args(headArg), env, recCont(remainArgs - 1, paramEnv + (params(headArg-1) -> argV)))
                                
                                }                                
                            } 
                            eval(args(0), env, recCont(args.length - 1, paramEnv))
                        }
                    }
                    case ContV(kk) =>   eval(args(0), env, kk)
                    case v => error(s"this is not Clov : $v")
                })
                case If0(cond, thenE, elseE) =>
                    eval(cond, env, _ match {
                        case NumV(0) => 
                            eval(thenE, env, k)
                        case _ =>
                            eval(elseE, env, k)
                    })
                case Withcc(name, body) =>
                    eval(body, env + (name -> ContV(k)), k)
                case Try(tryE, catchE) =>
                    val retV = eval(tryE, env, k)
                    retV match{
                        case ThrowV() =>
                            eval(catchE, env, k)
                        case _ =>
                            retV
                    }
                case Throw => ThrowV()
                
            }
        }
        eval(epr, defaultEnv, k) match {
            case NumV(n) => n.toString
            case CloV(params, body, env) => "function"
            case ContV(kk) => "continuation"
            case ThrowV() => error("no enclosing try-catch.")
        }
    }
    
    def ownTests(): Unit = {
        test(run("{{fun {x y} {- y x}} 10 12}"), "2")
        test(run("{fun {} 12}"), "function")
        test(run("{fun {x} {fun {} x}}"), "function")
        test(run("{{{fun {x} {fun {} x}} 13}}"), "13")
        test(run("{withcc esc {{fun {x y} x} 1 {esc 3}}}"), "3")

        print("----line 5----\n")
        test(run("{try 7 catch 8}"), "7")
   test(run("{try {throw} catch 8}"), "8")
   test(run("{try {+ 1 {throw}} catch 8}"), "8")
   test(run("{{fun {f} {try {f 3} catch 8}} {fun {x} {throw}}}"), "8")
   test(run("{try {try {throw} catch 8} catch 9}"), "8")
   test(run("{try {try {throw} catch {throw}} catch 9}"), "9")
   test(run("{try {try 7 catch {throw}} catch 9}"), "7")
   test(run("{{withcc esc {try {{withcc k {esc k}} 0} catch {fun {x} 8}}} {fun {x} {throw}}}"), "8")
        // multiple arguments [5]
   test(run("{{fun {x y} {- y x}} 10 12}"), "2")
   test(run("{fun {} 12}"), "function")
   test(run("{fun {x} {fun {} x}}"), "function")
   test(run("{{{fun {x} {fun {} x}} 13}}"), "13")
   test(run("{withcc esc {{fun {x y} x} 1 {esc 3}}}"), "3")

   // exceptions [35]
   test(run("{+ {withcc k {k 5}} 4}"), "9")
   test(run("{{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} 1 {+ y {g g {- y 1}}}}} 10}"), "55") // recursive function
   test(run("{withcc done {{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} {done 100} {+ y {g g {- y 1}}}}} 10}}"), "100") // exit from recursive function using continuation
   test(run("{withcc k {- 0 {k 100}}}"), "100")
   test(run("{withcc k {k {- 0 100}}}"), "-100")
   test(run("{withcc k {k {+ 100 11}}}"), "111")
   test(run("{{fun {a b c} {- {+ {withcc k {+ {k 100} a}} b} c}} 100 200 300}"), "0")
   test(run("{withcc esc {{fun {x y} x} 1 {esc 3}}}"), "3")
   test(run("{{withcc esc {{fun {x y} {fun {z} {+ z y}}} 1 {withcc k {esc k}}}} 10}"), "20")
   test(run("{try {{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} {throw} {+ y {g g {- y 1}}}}} 10} catch 110}"), "110") // exit from recursive function using try-catch
   test(run("{{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} {throw} {try {+ y {g g {- y 1}}} catch y}}} 10}"), "54") // equal? for multiple recursive try-catch
   test(run("{withcc done {{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} {throw} {try {+ y {g g {- y 1}}} catch {done y}}}} 10}}"), "2")
   test(run("{try {{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} {throw} {try {+ y {g g {- y 1}}} catch {throw}}}} 10} catch 20110464}"), "20110464") // recursive try-catch throwing ("1")
   test(run("{try {{fun {x y z} {a b c}} 1 2 {throw}} catch 0}"), "0")
   test(run("{{fun {f} {try {f 3} catch 8}} {fun {x} {throw}}}"), "8")
   test(run("{try {- 0 {withcc k {+ 3 {k {throw}}}}} catch 89}"), "89")
   test(run("{try {+ 3 {withcc k {+ 1000 {k {throw}}}}} catch 11}"), "11")
   test(run("{{fun {x y z} {try {+ 1 {+ x {throw}}} catch {+ y z}}} 1 2 3}"), "5")
   test(run("{+ {try {- 10 {throw}} catch 3} 10}"), "13")
   test(run("{try {if0 0 {throw} {+ 1 2}} catch {if0 10 1 {try {throw} catch 54}}}"), "54")
   test(run("{try {withcc a {+ 1 {withcc b {throw}}}} catch 10}"), "10")
   test(run("{try {- 0 {throw}} catch 5}"), "5")
   test(run("{try {if0 {throw} 3 4} catch 5}"), "5")
   test(run("{try {{fun {x y} {try x catch y}} {throw} 0} catch -1}"), "-1")
   test(run("{try {try {throw} catch {throw}} catch 9}"), "9")
   test(run("{{withcc esc {try {{withcc k {esc k}} 0} catch {fun {x} 8}}} {fun {x} {throw}}}"), "8")
   print("check\n")
   test(run("{{withcc esc {try {{withcc k {try {esc k} catch {fun {x} {fun {y} 9}}}} 0} catch {fun {x} 8}}} {fun {x} {throw}}}"), "8")
   test(run("{withcc foo {{fun {x y} {y x}} {+ 2 3} {withcc bar {+ 1 {bar foo}}}}}"), "5")
   test(run("{try {withcc zzz {{fun {x y z w} {+ {+ x y} {+ z w}}} 1 2 {zzz 10} {throw}}} catch 42}"), "10")
   test(run("{try {withcc zzz {{fun {x y z w} {+ {+ x y} {+ z w}}} 1 2 {throw} {zzz 10}}} catch 42}"), "42")
   test(run("{try {withcc zzz {{fun {x y z w} {+ {w {+ x y}} {+ {throw} z}}} 1 2 3 zzz}} catch 42}"), "3")
   test(run("{withcc esc {try {+ {throw} {esc 3}} catch 4}}"), "4")
   test(run("{withcc esc {{fun {x y} {+ {+ x 3} y}} {withcc k {try {k {esc {throw}}} catch {k 5}}} 7}}"), "15")
   test(run("{try {withcc x {+ {x 1} {throw}}} catch 0}"), "1")
   test(run("{+ 12 {withcc k {+ 1 {k {{fun {} 7}}}}}}"), "19")

   // multiple arguments [6]
   test(run("{+ 999 {withcc done {{fun {f x} {f f x done}} {fun {g y z} {if0 {- y 1} {z 100} {+ y {g g {- y 1} z}}}} 10}}}"), "1099")
   test(run("{+ 999 {withcc done {{fun {f x} {f f x {fun {x} {if0 x {done {- 0 999}} 10000}}}} {fun {g y z} {if0 {- y 1} {z 100} {+ y {g g {- y 1} z}}}} 10}}}"), "11053")
   test(run("{+ 999 {withcc done {{fun {f x} {f f x {fun {x} {if0 x {done {- 0 999}} 10000}}}} {fun {g y z} {if0 {- y 1} {z 0} {+ y {g g {- y 1} z}}}} 10}}}"), "0")
   test(run("{withcc done {{fun {f x} {f f x {fun {x} {if0 x {fun {y} {fun {x} {+ x y}}} 10000}}}} {fun {g y z} {if0 {- y 1} {z 0} {{g g {- y 1} z} 32}}} 3}}"), "64")
   test(run("{{withcc done {{fun {f x} {f f x {fun {x} {if0 x {withcc k {fun {x} {fun {x} {fun {x} k}}}} 10000}}}} {fun {g y z} {if0 {- y 1} {z 0} {{g g {- y 1} z} 32}}} 3}} 5}"), "continuation")
   test(run("{{withcc done {{fun {f x} {f f x {fun {x} {if0 x {withcc k {fun {x} {fun {x} {fun {x} k}}}} 10000}}}} {fun {g y z} {if0 {- y 1} {z 0} {{g g {- y 1} z} 32}}} 4}} {fun {y} {fun {y} {fun {y} {fun {x} 42}}}}}"), "42")

   // exceptions [4]
   test(run("{try {{withcc done {{fun {f x} {f f x {fun {x} {if0 x {withcc k {fun {x} {fun {x} {fun {x} k}}}} {throw}}}}} {fun {g y z} {if0 {- y 1} {z 1} {{g g {- y 1} z} 32}}} 4}} {fun {y} {fun {y} {fun {y} {fun {x} 42}}}}} catch 4242}"), "4242")
   test(run("{withcc esc {{try {withcc done {{fun {f x} {f f x {fun {x} {if0 x {withcc k {fun {x} {fun {x} {fun {x} k}}}} {throw}}}}} {fun {g y z} {if0 {- y 1} {z 1} {{g g {- y 1} z} 32}}} 4}} catch esc} 33}}"), "33")
   test(run("{try {try {throw} catch {try {throw} catch {try {throw} catch {+ {withcc k {try {throw} catch {k 0}}} {throw}}}}} catch 0}"), "0")
   test(run("{try {{withcc done {{fun {f x} {f f x {fun {x} {if0 x {withcc k {fun {x} {fun {x} {fun {x} k}}}} 10000}}}} {fun {g y z} {if0 {- y 1} {z 0} {{g g {- y 1} z} 32}}} 4}} {fun {y} {fun {y} {fun {y} {throw}}}}} catch 4242}"), "4242")

    }
}

{
    {withcc esc 
        {try 
            {{withcc k {try {esc k} catch {fun {x} {fun {y} 9}}}} 0} 
            catch {fun {x} 8}
        }
    } {fun {x} {throw}}
}