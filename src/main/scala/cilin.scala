import scala.io.{Source, StdIn}
import scala.math.Pi
import scala.collection.mutable.{Map, Set}

class CilinSimilarity {
  val a: Double = 0.65
  val b: Double = 0.8
  val c: Double = 0.9
  val d: Double = 0.96
  val e: Double = 0.5
  val f: Double = 0.1
  val pi: Double = Pi
  val degree: Double = 180
  var N: Int = 0

  val stream = getClass.getResourceAsStream("/cilin.txt")
  val cilin: List[String] = Source.fromInputStream(stream).getLines.toList
  val CodeWord: Map[String, List[String]] = Map[String, List[String]]()
  val WordCode: Map[String, List[String]] = Map[String, List[String]]()
  val Vocab: Set[String] = Set[String]()

  cilin.foreach({
    case line =>
      val res = line.split(" ")
      val code = res(0)
      val words = res.slice(1, res.size).toList

      CodeWord += (code -> words)
      words.foreach(word => {
        if (!WordCode.exists(_._1 == word)) {
          WordCode += (word -> List[String]())
        }
        WordCode(word) ::= code
      })

      words.foreach(Vocab.add(_))
      N += words.size
  })

  def similarity(w1: String, w2: String): Double = {
    if (!Vocab.exists(_ == w1) || !Vocab.exists(_ == w2)) {
      0
    } else {
      val code1 = WordCode(w1)
      val code2 = WordCode(w2)

      var best = 0.0
      code1.foreach(c1 => {
        code2.foreach(c2 => {
          best = best max (similarityByCode(c1, c2))
        })
      })
      best
    }
  }

  def similarityByCode(c1: String, c2: String): Double = {
    val clayer1 = codeLayer(c1)
    val clayer2 = codeLayer(c2)

    val commonPart = common(c1, c2)
    if (c1.endsWith("@") || c2.endsWith("@") || 0 == commonPart.size) {
      return f
    }
    val sim = {
      if (commonPart.size >= 7) {
        if (c1.endsWith("=") && c2.endsWith("=")) 1
        else if (c1.endsWith("#") && c2.endsWith("#")) e 
        else 0.0
      } else {
        val k = getK(clayer1, clayer2)
        val n = getN(commonPart)
        commonPart.size match {
          case 1 => formula(a, n, k)
          case 2 => formula(b, n, k)
          case 4 => formula(c, n, k)
          case 5 => formula(d, n, k)
          case _ => 0.0
        }
      }
    }
    sim.toDouble
  }

  def formula(coffe: Double, n: Int, k: Int): Double = {
    coffe * Math.cos(n.toDouble * pi.toDouble / degree.toDouble) * ((n - k + 1).toDouble / n)
  }

  def codeLayer(c: String): List[String] = {
    List[String](c.slice(0, 1), c.slice(1, 2), c.slice(2, 4), c.slice(4, 5), c.slice(5, 7), c.slice(7, 8))
  }

  def getLayer(str: String): Int = {
    str.length match {
      case 1 => 1
      case 2 => 2
      case 4 => 3
      case 5 => 4
      case 7 => 5
      case _ => 0
    }
  }

  def common(s1: String, s2: String): String = {
    val z = s1 zip s2
    val c = new StringBuilder
    var b = true
    z.foreach(v => {
      if (v._1 == v._2 && b) c.append(v._1)
      else b = false
    })
    if (c.size == 3 || c.size == 6) {
      c.toString.slice(0, c.size - 1)
    } else {
      c.toString
    }
  }

  def getK(c1: List[String], c2: List[String]): Int = {
    if (c1(0) != c2(0)) {
      Math.abs(c1(0).head - c2(0).head)
    } else if (c1(1) != c2(1)) {
      Math.abs(c1(1).head - c2(1).head)
    } else if (c1(2) != c2(2)) {
      Math.abs(c1(2).toInt - c2(2).toInt)
    } else if (c1(3) != c2(3)) {
      Math.abs(c1(3).head - c2(3).head)
    } else {
      Math.abs(c1(4).toInt - c2(4).toInt)
    }
  }

  def getN(str: String): Int = {
    if (0 == str.size) {
      return 0
    }
    val s = Set[String]()
    val layer = getLayer(str)
    println(layer)
    CodeWord.foreach({
      case (k, _) =>
        if (k.startsWith(str)) {
          val clayer = codeLayer(k)
          s.add(clayer(layer))
        }
    })
    s.size
  }
}

object Cilin {
  def main(args: Array[String]) {
    val cs = new CilinSimilarity
    for (i <- 1 to 10000) {
      val line = StdIn.readLine()
      val p = line.split(" ")
      val sim = cs.similarity(p(0), p(1))
      println(sim)
    }
    
  }
}