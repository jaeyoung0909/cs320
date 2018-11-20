import Util._

object Main extends Homework02 {
    def run(str: String): List[Int] = {
        val epr = MUWAE.apply(str)
        val defaultEnv = Map[String, List[Int]]()

        def eval(epr : MUWAE, env: Map[String, List[Int]]): List[Int] = {
            def binOp(
                op: (Int, Int) => Int,
                ls: List[Int],
                rs: List[Int]
            ): List[Int] = ls match {
                case Nil => Nil
                case l :: rest =>
                def f(r: Int): Int = op(l, r)
                rs.map(f) ++ binOp(op, rest, rs)
            }    

            def add(a: Int, b: Int) : Int = {a + b}
            def sub(a: Int, b: Int) : Int = {a - b}
            
            epr match {
                case Num(n) => n
                case Add(l, r) =>  binOp(add, eval(l, env), eval(r, env))
                case Sub(l, r) => binOp(sub, eval(l, env), eval(r, env))
                case With(name, exp, body) => 
                    val evalExp: List[Int] = eval(exp, env)
                    val updatedEnv: Map[String, List[Int]] = env + (name -> evalExp)
                    eval(body, updatedEnv)
                case Id(id) => if (!env(id).isEmpty) env(id) else error("evaluation fail : free identifier")
                case Min(l, m, r) => List(List(eval(l, env).head, eval(m, env).head, eval(r, env).head).min)
                case Max(l, m, r) => List(List(eval(l, env).head, eval(m, env).head, eval(r, env).head).max)
            }
        }
        eval(epr, defaultEnv)
    }

    def ownTests(): Unit = {
        testExc(run("{x}"), "bad syntax")
        testExc(run("{+ x x}"), "key not found")
        testExc(run("{with {x 3} {+ y y}}"), "key not found")

        test(run("{+ 5 10}"), List(15))
        test(run("{- 10 {10 20}}"), List(0, -10))
        test(run("{with {x {+ 1 5}} {+ x x}}"), List(12))
        test(run("{muwae-min 9 4 5}"), List(4))
        test(run("{muwae-max {+ 15 2} 4 5}"), List(17))

        test(run("{+ {muwae-min 1 3 7} {muwae-max 60 2 20}}"), List(61))
        test(run("{+ {6 2} {1 4}}"), List(7, 10, 3, 6))
        test(run("{- {10 2 1} {3 1}}"), List(7, 9, -1, 1, -2, 0))
        test(run("{with {x {2 2}} {+ x {4 3}}}"), List(6, 5, 6, 5))
        test(run("{with {x 1} {+ x {with {x 3} x}}}"), List(4))

        test(run("{with {x 10} {+ x {with {y 3} x}}}"), List(20))
        test(run("{with {x 10} {+ x {with {x 3} 10}}}"), List(20))
        test(run("{with {x {2 5}} {+ x x}}"), List(4, 7, 7, 10))
        test(run("{with {x 20} {with {y 5} {with {z {10 20}} {+ z {muwae-max {- x y} 0 12}}}}}"), List(25, 35))
        test(run("{with {x 20} {with {y 5} {with {z {10 20}} {+ z {muwae-min {- x y} 0 12}}}}}"), List(10, 20))

        test(run("{with {x {8}} {muwae-min x {3} {5}}}"), List(3))
        test(run("{with {x {4}} {muwae-max x {3} {5}}}"), List(5))
        test(run("{muwae-min {300} 400 500}"), List(300))
        test(run("{muwae-max {38} 41 {55}}"), List(55))
        test(run("{+ {10 100 1000 10000} {muwae-min {+ 3 4} 5 6}}"), List(15, 105, 1005, 10005))
    }
}