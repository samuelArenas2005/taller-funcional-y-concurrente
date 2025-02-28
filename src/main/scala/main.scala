@main
def unit(): Unit = {
  def peasantAlgorithm(a: Int, b: Int, result: Int): Int = {

    def isOdd: Int = peasantAlgorithm(a / 2, b + b, result)
    def isOven: Int = peasantAlgorithm(a / 2, b + b, result + b)

    if (a != 0) if (a%2==0) isOdd else isOven else result
  }

  def splitAlgorithm(a: Int, b: Int): Int = {

    def numberDigits(c: Int, nDigits: Int): Int = if(c == 0)  nDigits  else numberDigits(c / 10, nDigits + 1);

    if(a < 10 && b < 10) return a*b

    val numberDigitA: Int = numberDigits(a, 0) / 2
    val numberDigitB: Int = numberDigits(b, 0) / 2
    val nFinal: Int = if (numberDigitA >= numberDigitB) numberDigitA else numberDigitB //PREGUNTAR AL PROFESOR...

    def defTopPart(d: Int, nDigits: Int): Int = d / Math.pow(10, nDigits).toInt;
    def defBottomPart(e: Int, numberDigit: Int): Int = e % math.pow(10, numberDigit).toInt;;

    val topPartA = defTopPart(a,nFinal)
    val bottomPartA = defBottomPart(a,nFinal)
    val topPartB = defTopPart(b, nFinal)
    val bottomPartB = defBottomPart(b, nFinal)

    def firstStep: Int = Math.pow(10, nFinal*2).toInt * splitAlgorithm(topPartA,topPartB)
    def secondStep: Int = Math.pow(10, nFinal).toInt * ( splitAlgorithm(bottomPartA,topPartB) + splitAlgorithm(bottomPartB,topPartA) )
    def thirdStep: Int = splitAlgorithm(bottomPartA,bottomPartB)

    def algorithmMethod(): Int = firstStep + secondStep + thirdStep

    algorithmMethod();
  }

  println(splitAlgorithm(20,-5))


  def suma(a:Int): Int => Int = {
    x => x +a
  }
  print(suma(5)(10))

}