import Util._ 

object Main extends Homework07 {
    trait CORELValue
    case class NumV(n: Int) extends CORELValue 
    case class CloV(params: String, body: COREL, env: Map[String, CORELValue]) extends CORELValue 
    case class VariantV(name: String, value: CORELValue) extends CORELValue 
    case class ConstructorV(name: String) extends CORELValue 
    case class TyEnv(
        vars    : Map[String, Type] = Map(),
        tbinds  : Map[String, Map[String, Type]] = Map()
    ) {
        def addVar(x: String, t: Type): TyEnv =
            copy(vars = vars + (x -> t))
        def addTBind(x: String, cs: Map[String, Type]): TyEnv =
            copy(tbinds = tbinds + (x -> cs))
    }

    def typeCheck(str: String): Type = {
        val e = COREL.apply(str)
        val defaulttyEnv = Map[String, Type]()

        def mustSame(left: Type, right: Type): Type = 
            if (same(left, right)) left 
            else notype(s"$left is not equal to $right")
        def same(left: Type, right: Type): Boolean = 
            (left, right) match {
                case (NumT, NumT) => true 
                case (ArrowT(p1, r1), ArrowT(p2, r2)) =>
                    same(p1, p2) && same(r1, r2)
                case _ => false
            }

        def notype(msg: Any): Nothing = error(s"notype: $msg")

        def validType(ty: Type, tyEnv: TyEnv): Type = ty match {
            case NumT => ty 
            case ArrowT(p, r) =>
                ArrowT(validType(p, tyEnv), validType(r, tyEnv))
            case IdT(x) =>
                if (tyEnv.tbinds.contains(x)) ty 
                else notype(s"$x is a free type")
        }

        def typeChecker(epr : COREL, tyEnv: TyEnv): Type = {
            epr match{
                case Num(n) => NumT
                case Bool(b) => BoolT
                case Add(l,r) =>
                    mustSame(typeChecker(l, tyEnv), NumT)
                    mustSame(typeChecker(r, tyEnv), NumT)
                case Sub(l,r) =>
                    mustSame(typeChecker(l, tyEnv), NumT)
                    mustSame(typeChecker(r, tyEnv), NumT)
                case Equ(l,r) =>
                    mustSame(typeChecker(l, tyEnv), NumT)
                    mustSame(typeChecker(r, tyEnv), NumT)
                    BoolT
                case Id(s) =>
                    tyEnv.getOrElse(x, notype(s"$x is a free identifier"))
                case Fun(x, t, b) =>
                    validType(t, tyEnv)
                    ArrowT(t, typeChecker(b, tyEnv + (x->t)))
                case App(f, a) =>
                    val funT = typeChecker(f, tyEnv)
                    val argT = typeChecker(a, tyEnv)
                    funT match {
                        case ArrowT(param, result)
                            if same(argT, param) => result
                        case _ => notype(s"apply $argT to $funT")
                    }
                case IfThenElse(testE,thenE,elseE) =>
                    mustSame(typeChecker(testE, tyEnv), BoolT)
                    mustSame(typeChecker(thenE, tyEnv), typeChecker(elseE, tyEnv))
                case Rec(fname, fty, pname, pty, body) =>
                    mustSame(ft,
                            ArrowT(xt, typeChecker(b, tyEnv + (f -> tf, x-> xt))))
                case WithType(name, constructors, body) =>
                    val tyEnvT = tyEnv.addTBind(tn, Map(vfn -> vft, vsn -> vst))
                    val tyEnvV = tyEnvT.addVar(vfn, ArrowT(vft, IdT(tn)))
                                        .addVar(vsn, ArrowT(vst, IdT(tn)))
                    val t = typeChecker(b, tyEnvV) match {
                        case ArrowT(param, result)
                            if(same(param, t) || same(result, t)) => notype(s"$t makes cycle type")
                        case  _ => t
                    }
                case Cases(name, dispatchE, cases) =>
                    val cs = tyEnv.tbinds.getOrElse(tn, notype(s"$tn is a free type"))
                    mustSmae(typeChecker(c, tyEnv), IdT(tn))
                    val rft = typeChecker(rfe, 
                                        tyEnv.addVar(bfn, 
                                                    cs.getOrElse(vfn,
                                                                notype(s"$vfn is free"))))
                    val rst = typeChecker(rse,
                                        tyEnv.addVar(bsn,
                                                    cs.getOrElse(vsn,
                                                                notype(s"$vsn is free"))))
                    mustSame(rft, rst)
                case TFun(name, expr) =>
                    PolyT(name, typeChecker(expr, tyfun.addVar(name, )))
                case TApp(body, ty) =>
            }
        }
    }
        
    

    def run(str: String): String = {
        val epr = COREL.apply(str)
        val defaultEnv = Map[String, CORELValue]()

        def eval(epr : COREL, env: Map[String, CORELValue]): CORELValue = {   

            def add(a: CORELValue, b: CORELValue) : CORELValue = (a, b) match {
                case (NumV(numA), NumV(numB)) => NumV(numA + numB)
                case _ => error("can not add CORELValue") 
            }
            def sub(a: FWAEValue, b: FWAEValue) : FWAEValue = (a, b) match {
                case (NumV(numA), NumV(numB)) => NumV(numA - numB)
                case _ => error("can not add CORELValue") 
            }
            epr match {
                case Num(n) => NumV(n)
                case Add(l, r) => add(eval(l, env), eval(r, env))
                case Sub(l, r) => sub(eval(l, env), eval(r, env))
                case With(name, exp, body) =>
                    if (name == "fun" || name == "with")    error("id name can not be with or fun") 
                    val evalExp: CORELValue = eval(exp, env)
                    val updatedEnv: Map[String, CORELValue] = env + (name -> evalExp)
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
            }
        }
        eval(epr, defaultEnv) match {
            case NumV(n) => n.toString
            case CloV(params, body, env) => "function"
        }
    }
    
    def ownTests(): Unit = {
        print(typeCheck("{num->bool}"))
    }
} 