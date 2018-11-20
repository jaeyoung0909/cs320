import Util._ 

object Main extends Homework04 {
    trait BFAEValue
    case class NumV(n: Int) extends BFAEValue
    case class CloV(param: String, body: BFAE, env: Map[String, Int]) extends BFAEValue
    case class Record(rec: Map[String, BFAEValue], addr: Int) extends BFAEValue
    case class BoxV(addr: Int) extends BFAEValue
    def run(str: String): String = {
        val exp = BFAE.apply(str)
        val defaultEnv = Map[String, Int]()
        val defaultSto = Map[Int, BFAEValue]()

        def eval(exp: BFAE, env: Map[String, Int], sto: Map[Int, BFAEValue]) : (BFAEValue, Map[Int, BFAEValue]) = {
            def add(l: BFAEValue, r: BFAEValue) : BFAEValue = (l, r) match {
                case (NumV(numl), NumV(numr)) => NumV(numl + numr)
                case _ => error("only add NumV")
            }

            def sub(l: BFAEValue, r: BFAEValue) : BFAEValue = (l, r) match {
                case (NumV(numl), NumV(numr)) => NumV(numl - numr)
                case _ => error("only sub NumV")
            }

            def malloc(sto: Map[Int, BFAEValue]) : Int = {
                maxAddr(sto) + 1
            }

            def maxAddr(sto: Map[Int, BFAEValue]) : Int = {
                sto.keySet.+(0).max
            }

            exp match {
                case Num(n) => (NumV(n), sto)
                case Add(l, r) => 
                    val (vl, stol) = eval(l, env, sto)
                    val (vr, stor) = eval(r, env, stol)
                    (add(vl, vr), stor)
                case Sub(l, r) =>
                    val (vl, stol) = eval(l, env, sto)
                    val (vr, stor) = eval(r, env, stol)
                    (sub(vl, vr), stor)
                
                case Id(s) => env.keySet.exists(_ == s) match{
                    case true => (sto(env(s)), sto)
                    case false => error("free identifier error")
                }
                case Fun(param, body) => (CloV(param, body, env), sto)
                case App(func, argu) => 
                    val (funcV, funcSto) = eval(func, env, sto)
                    funcV match {
                        case CloV(param, body, fenv) =>
                            val (arguV, arguSto) = eval(argu, env, funcSto)
                            arguV match {
                                case Record(rec, arguAddr) => 
                                    eval(body, fenv + (param -> arguAddr), arguSto + (arguAddr->arguV))
                                case _ =>
                                    val addr = malloc(arguSto)
                                    eval(body, fenv + (param -> addr), arguSto + (addr->arguV))
                            }
                        case others => error(s"this is not CloV : $others")
                    }
                case NewBox(expr) =>
                    val (v, s) = eval(expr, env, sto)
                    val addr = malloc(s)
                    (BoxV(addr), s + (addr -> v))
                case SetBox(box, expr) =>  
                    val (vb, sb) = eval(box, env, sto)
                    vb match {
                        case BoxV(addr) => 
                            val (ve, se) = eval(expr, env, sb)
                            (ve, se + (addr -> ve))
                        case _ => error("first parameter of SetBox must be BoxV")
                    }
                case OpenBox(box) =>
                    val (vb, sb) = eval(box, env, sto)
                    vb match {
                        case BoxV(addr) =>
                            (sb(addr), sb)
                        case _ => error("first parameter of OpenBox must be BoxV")
                    }
                case Seqn(l, r) =>
                    val (vl, sl) = eval(l, env, sto)
                    r.foldLeft((vl, sl)) { (s: (BFAEValue, Map[Int, BFAEValue]), r: BFAE) =>
                                                eval(r, env, s._2)}
                    
                case Rec(field) => 
                    val addr = malloc(sto)
                    val newSto = sto + ( addr -> Record(Map[String, BFAEValue](), addr) )
                    val lastSto = field.foldLeft(newSto){ (s: Map[Int, BFAEValue], r: (String, BFAE)) =>
                                            val (lv, ls) = eval(r._2, env, s)
                                            ls + (addr -> Record( ls(addr) match {
                                                    case Record(rec, addr) => rec + (r._1 -> lv)
                                                }, addr))
                                            }
                    (lastSto(addr), lastSto)                
                case Get(expr, field) => 
                    val (v, s) = eval(expr, env, sto)
                    v match {
                        case Record(rec, addr) => (rec.get(field).getOrElse(error("no such field")), s)
                        case _ =>
                            error("First parameter of Get must be Record")
                    }
                case Set(record, field, expr) =>
                    val (rv, rs) = eval(record, env, sto)
                    val (ev, es) = eval(expr, env, rs)
                    rv match{
                        case Record(rec, addr) => 
                            if(!rec.keySet.exists(_ == field))  error("no such field")
                            val newSto = es + (addr -> Record(rec + (field -> ev), addr))
                            (ev, newSto)
                        case _ => error("First parameter of Set must be Record")
                    }
            }    

            
        }
        val (v, s) = eval(exp, defaultEnv, defaultSto) 
        v match {
            case NumV(n) => n.toString
            case CloV(param, body, env) => "function"
            case Record(field, addr) => "record"
            case BoxV(addr) => "box"
        }
    }
    def ownTests(): Unit = {
        testExc(run("{+ a b}"), "free identifier")
        testExc(run("{get {rec {a 10} {b {+ 1 2}}} c}"), "no such field")
        testExc(run("{set {rec {a 10} {b {+ 1 2}}} c 4}"), "no such field")
        
        test(run("""{{fun {x} {seqn {setbox x {+ 2 {openbox x}}}
                               {setbox x {+ 4 {openbox x}}}
                               {openbox x}}}
                {newbox 100}}"""), "106")
        test(run("{seqn {+ 1 3} {- 2 1}}"), "1")
        test(run("{{fun {a} {openbox a}} {newbox 7}}"), "7")
        test(run("{{fun {a} {seqn {setbox a 7} {openbox a}}} {newbox 10}}"), "7")
        test(run("{{fun {b} {openbox b}} {seqn {newbox 1} {newbox 2}}}"), "2")

        test(run("{{{fun {b} {fun {a} {openbox b}}} {newbox 90}} {newbox 101}}"), "90")
        test(run("{{fun {a} {seqn {setbox a 20} {openbox a}}} {newbox 1}}"), "20")
        test(run("{{fun {r} {get r a}} {rec {s {- 1 2}} {a {+ 5 2}}}}}"), "7")
        test(run("{{fun {r} {seqn {set r x 50} {get r x}}} {rec {x 1}}}"), "50")
        test(run("{{{{{fun {f} {fun {s} {fun {r1} {fun {r2} {+ {get r1 b} {seqn {{s r1} {f r2}} {+ {seqn {{s r2} {f r1}} {get r1 b}} {get r2 b}}}}}}}} {fun {r} {get r a}}} {fun {r} {fun {v} {set r b v}}}} {rec {a 1} {b 2}}} {rec {a 3} {b 4}}}"), "6")       
        
        test(run("3"), "3")
        test(run("{fun {x} {+ x 3}}"), "function")
        test(run("{newbox 100}"), "box")
        test(run("{rec {a {+ 1 2}} {b 3}}"), "record")
	test(run("""{{fun {b} {seqn {setbox b {+ 2 {openbox b}}}
            {setbox b {+ 3 {openbox b}}}
            {setbox b {+ 4 {openbox b}}}
            {openbox b}}}
            {newbox 1}}"""), "10")
/*02*/test(run("{seqn 1 2}"), "2")
/*03*/test(run("{{fun {b} {openbox b}} {newbox 10}}"), "10")
/*04*/test(run("{{fun {b} {seqn {setbox b 12} {openbox b}}} {newbox 10}}"), "12")
/*05*/test(run("{{fun {b} {openbox b}} {seqn {newbox 9} {newbox 10}}}"), "10")
/*06*/test(run("{{{fun {b} {fun {a} {openbox b}}} {newbox 9}} {newbox 10}}"), "9")
/*07*/test(run("{{fun {b} {seqn {setbox b 2} {openbox b}}} {newbox 1}}"), "2")
/*08*/test(run("{{fun {b} {seqn {setbox b {+ 2 {openbox b}}} {setbox b {+ 3 {openbox b}}} {setbox b {+ 4 {openbox b}}} {openbox b}}} {newbox 1}}"), "10")
/*09*/test(run("{{fun {r} {get r x}} {rec {x 1}}}"), "1")
/*10*/test(run("{{fun {r} {seqn {set r x 5} {get r x}}} {rec {x 1}}}"), "5")
/*11*/test(run("{{{{{fun {g} {fun {s} {fun {r1} {fun {r2} {+ {get r1 b} {seqn {{s r1} {g r2}} {+ {seqn {{s r2} {g r1}} {get r1 b}} {get r2 b}}}}}}}} {fun {r} {get r a}}} {fun {r} {fun {v} {set r b v}}}} {rec {a 0} {b 2}}} {rec {a 3} {b 4}}}"), "5")

/*12*/test(run("{seqn {{fun {b} {seqn {setbox b {+ 2 {openbox b}}} {setbox b {+ 3 {openbox b}}} {setbox b {+ 4 {openbox b}}} {openbox b}}} {newbox 1}} {seqn 1 2} {{fun {b} {openbox b}} {newbox 10}} {{fun {b} {seqn {setbox b 12} {openbox b}}} {newbox 10}} {{fun {b} {openbox b}} {seqn {newbox 9} {newbox 10}}} {{{fun {b} {fun {a} {openbox b}}} {newbox 9}} {newbox 10}}}"), "9")
/*13*/test(run("{seqn 13 {{fun {x} x} {fun {x} x}}}"), "function")
/*14*/test(run("{seqn 14 {{fun {y} y} {newbox 1}}}"), "box")
/*15*/test(run("{seqn 15 {{fun {z} z} {rec}}}"), "record")
/*16*/test(run("{{fun {z} z} {rec {x 0}}}"), "record")
/*17*/testExc(run("{{fun {z} z} {rec {x z}}}"), "")
/*18*/test(run("{{fun {z} {get {rec {x z}} x}} 1}"), "1")
/*19*/test(run("{{get {{fun {z} {rec {x {fun {y} z}}}} 1} x} 2}"), "1")
/*20*/testExc(run("{{get {{fun {z} {rec {x {fun {y} x}}}} 1} x} 2}"), "")
/*21*/testExc(run("{{get {{fun {z} {rec {x {fun {y} x}}}} 1} y} 2}"), "no such field")
/*22*/test(run("{{fun {z} {seqn {{fun {a} {seqn a {openbox a} {setbox a 3} {set z y a} {setbox {get z y} 0} {openbox a}}} {get z x}}}} {rec {x {newbox 1}} {y {newbox 2}}}}"), "0")
/*23*/test(run("{{fun {z} {seqn {{fun {a} {seqn a {openbox a} {setbox a 3} {set z y a} {setbox {get z y} 0} {openbox a}}} {get z x}} {set z x z} {set z y {newbox 42}} {openbox {get {get {get {get {get z x} x} x} x} y}}}} {rec {x {newbox 1}} {y {newbox 2}}}}"), "42")
/*24*/test(run("{{fun {f} {{fun {x} {seqn {setbox x {{f {newbox 2}} {newbox 1}}} {openbox {get {openbox x} x}}}} {newbox 100}}} {fun {y} {fun {x} {rec {x x} {y y} {z {newbox 30}}}}}}"), "1")
/*25*/test(run("{{fun {f} {{fun {x} {seqn {setbox x {{f {newbox 2}} {newbox 1}}} {openbox {get {openbox x} x}} {setbox {get {openbox {get {get {{f {newbox 22}} {rec {x x}}} x} x}} y} 22} {openbox {get {openbox x} y}}}} {newbox 100}}} {fun {y} {fun {x} {rec {x x} {y y} {z {newbox 30}}}}}}"), "22")
    }
}
