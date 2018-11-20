import Util._ 

object Main extends Homework03 {
    trait FWAEValue
    case class NumV(n: Int) extends FWAEValue 
    case class CloV(params: List[String], body: FWAE, env: Map[String, FWAEValue]) extends FWAEValue 
    case class Record(rec: Map[String, FWAEValue]) extends FWAEValue

    def run(str: String): String = {
        val epr = FWAE.apply(str)
        val defaultEnv = Map[String, FWAEValue]()

        def eval(epr : FWAE, env: Map[String, FWAEValue]): FWAEValue = {   

            def add(a: FWAEValue, b: FWAEValue) : FWAEValue = (a, b) match {
                case (NumV(numA), NumV(numB)) => NumV(numA + numB)
                case _ => error("can not add FAWEValue") 
            }
            def sub(a: FWAEValue, b: FWAEValue) : FWAEValue = (a, b) match {
                case (NumV(numA), NumV(numB)) => NumV(numA - numB)
                case _ => error("can not add FAWEValue") 
            }
            epr match {
                case Num(n) => NumV(n)
                case Add(l, r) => add(eval(l, env), eval(r, env))
                case Sub(l, r) => sub(eval(l, env), eval(r, env))
                case With(name, exp, body) =>
                    if (name == "fun" || name == "with")    error("id name can not be with or fun") 
                    val evalExp: FWAEValue = eval(exp, env)
                    val updatedEnv: Map[String, FWAEValue] = env + (name -> evalExp)
                    eval(body, updatedEnv)
                case Id(id) => if (!(env.get(id) == None)) env(id) else error("evaluation fail : free identifier")
                case App(func, args) => eval(func, env) match {
                    case CloV(params, body, fenv) => {
                        if (params.length != args.length)    error("wrong arity")

                        val evaluatedArgs : List[FWAEValue] = args.map(eval(_, env))
                        def matchParams2Args(params : List[String], args : List[FWAEValue]): Map[String, FWAEValue] = {
                            params match {
                                case Nil => Map[String, FWAEValue]()
                                case h::rest => matchParams2Args(rest, args.tail) + (h -> args.head)
                            }
                        }
                        val updatedFenv : Map[String, FWAEValue] = fenv ++ matchParams2Args(params, evaluatedArgs)
                        eval(body, updatedFenv)
                    }
                    case v => error(s"this is not Clov : $v")
                }
                case Fun(args, body) => CloV(args, body, env)
                case Rec(rec) => Record(rec.mapValues(eval(_, env)))
                case Acc(expr, name) => eval(expr, env) match {
                    case Record(rec) => if (!(rec.get(name) == None)) rec(name) else error("no such field")
                    case _ => error("access must find on record")
                }
            }
        }
        eval(epr, defaultEnv) match {
            case NumV(n) => n.toString
            case CloV(params, body, env) => "function"
            case Record(rec) => "record"
        }
    }
    
    def ownTests(): Unit = {
        testExc(run("{with {g {fun {r} {+ r 1}}} {g 3 4}}"), "wrong arity")
        testExc(run("{access {record {a 10} {b {+ 1 2}}} c}"), "no such field")
        testExc(run("{with {with 3} {+ with with}}"), "with")
        testExc(run("{with {fun 3} {+ fun fun}}"), "fun")
        testExc(run("{+ a b}"), "free identifier")

        test(run("{fun {r} {+ r r}}"), "function")
        test(run("{with {g {fun {r} {+ r 1}}} {g 3}}"), "4")
        test(run("{access {access {record {g {record {f 110}}}} g} f}"), "110")
        test(run("{record {x 1} {y {+ 100 21}}}"), "record")
        test(run("{with {f {fun {x y} {record {a 3} {b y} {c x} {d 2} {e 3}}}} {access {f 100 200} b}}"), "200")
        
        test(run("{with {x 10} {+ x {with {y 3} x}}}"), "20")
        test(run("{access {record {a {+ 30 100}}} a}"), "130")
        test(run("{with {x {record {a 1} {b 2}}} {access x a}}"), "1")
        test(run("{with {g {fun {x} {access x q}}} {g {record {p 10} {q 121} {r 70}}}}"), "121")
        test(run("{record {x 1} {y {+ 10000 100000}}}"), "record")
    }
} 