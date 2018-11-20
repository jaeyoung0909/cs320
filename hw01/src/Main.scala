import Util._

object Main extends Homework01 {
  /* TODO Implement 10 missing functions */
  def dollar2won(dollar:Int): Int = {
    val won = dollar * (1100)
    if (dollar > 0) won  
    else error("non-positive value is given")
  }
  def volumeOfCuboid(a: Int, b: Int, c: Int): Int = {
    val volume = a * b * c
    if (a > 0 && b>0&&c>0) volume
    else error("non-positive value is given")
  }

  def isEven(num: Int): Boolean = {
    (num % 2) == 0
  }

  def isOdd(num: Int): Boolean = {
    (num % 2) == 1 || (num % 2) == -1
  }

  def gcd(a: Int, b: Int): Int = {
    val inputA = if(a < 0) -a else a 
    val inputB =  if(b < 0) -b else b
    if (inputA ==0 || inputB == 0)  return inputA.max(inputB)
    else gcd(inputA.max(inputB) - inputA.min(inputB), inputA.min(inputB))    
  }

  def lcm(a: Int, b: Int): Int = {
    val ret = a * b / gcd(a, b)
    return if (ret < 0) -ret else ret
  }

  def numOfHomework(course: COURSE): Int = course match{
    case CS320(q, h) => h 
    case CS311(h) => h
    case CS330(p, h) => h
  }

  def hasProjects(course: COURSE): Boolean = course match{
    case CS330(p, h) => (p >= 2)
    case _ => false
  }

  def namePets(pets: List[String]): List[String] = {
    pets.map(name => name match {
      case "dog" => "happy"
      case "cat" => "smart"
      case "pig" => "pinky"
      case _ => name
    })
  }

  def giveName(oldName: String, newName: String): List[String] => List[String] = {
    def old2new(pets: List[String]): List[String] = {
      pets.map(name => name match {
        case `oldName` => `newName` 
        case _ => name 
      })
    }
    return old2new
  }

  def ownTests(): Unit = {
    /* TODO Write your own tests */
    println("function 1 : dollar2won")
    test(dollar2won(1), 1100)
    test(dollar2won(3), 3300)
    testExc(dollar2won(-1), "non-positive")
    println("function 2 : volumeOfCuboid")
    test(volumeOfCuboid(1, 2, 3),6)
    test(volumeOfCuboid(3, 4, 5), 60)
    testExc(volumeOfCuboid(-1,2,3), "non-positive")
    println("function 3 : isEven")
    test(isEven(-6),true)
    test(isEven(3), false)
    println("function 4 : isOdd")
    test(isOdd(-7), true)
    test(isOdd(4), false)
    println("function 5 : gcd")
    test(gcd(3, 5), 1)
    test(gcd(15, 20), 5)
    test(gcd(-15, -20), 5)
    test(gcd(-70, 21), 7)
    println("function 6 : lcm")
    test(lcm(2, 3), 6)
    test(lcm(15, 20), 60)
    test(lcm(-15, -20), 60)
    test(lcm(-50, 25), 50)
    println("function 7 : numOfHomework")
    test(numOfHomework(CS320(3, 5)), 5)
    test(numOfHomework(CS330(2, 7)), 7)
    println("function 8 : hasProjects")
    test(hasProjects(CS330(1, 2)), false)
    test(hasProjects(CS330(4, 5)), true)
    test(hasProjects(CS311(5)), false)
    println("function 9 : namePets")
    test(namePets(List()), List())
    test(namePets(List("lion", "tiger", "cat")), List("lion", "tiger", "smart"))
    test(namePets(List("human", "bear", "pig")), List("human", "bear", "pinky"))
    println("function 10 : giveName")
    val nameLions = giveName("lion", "honey")
    test(nameLions(List()), List())
    test(nameLions(List("human", "lion", "cat", "bear")), List("human", "honey", "cat", "bear"))
  }
}
