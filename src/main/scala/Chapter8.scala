import doodle.core.*
import doodle.image.*
import doodle.image.syntax.all.*
import doodle.image.syntax.core.*
import doodle.java2d.*

object Chapter8 {
  ////////////////////////////////////////////////
  def square(color: Color): Image =
    Image.square(30).fillColor(color)

  def blockOfFour(): Image =
    square(Color.red).beside(square(Color.black)).above(square(Color.black).beside(square(Color.red)))

  def chessboard(count: Int): Image =
    count match
      case 0 => blockOfFour()
      case n =>
        val unit = chessboard(n - 1)
        unit.beside(unit).above(unit.beside(unit))

  //////////////////////////////////////////////////
  def triangle(): Image =
    Image.triangle(20, 20).strokeColor(Color.darkCyan)

  def blockOfThree(): Image =
    triangle().above(triangle().beside(triangle()))

  def sierpinski(count: Int): Image =
    count match {
      case 0 => blockOfThree()
      case n =>
        val unit = sierpinski(n - 1)
        unit.above(unit.beside(unit))
    }

  /////////////////////////////////////////////////
  def aBox(size: Int, color: Color): Image =
    Image.square(size).fillColor(color)

  def gradientBoxes(count: Int, color: Color): Image =
    val boxSize = 50
    count match
      case 0 => Image.empty
      case n => aBox(boxSize, color).beside(gradientBoxes(n - 1, color.spin(10.degrees) ))

  ///////////////////////////////////////////////////
  def aCircle(size: Int): Image =
    Image.circle(size).strokeColor(Color.blue)

  def concentricCircles(count: Int, size: Int): Image =
    count match
      case 0 => Image.empty
      case n => aCircle(size).on(concentricCircles(n-1, size + 10))

  ////////////////////////////////////////////////////
  def aCircleWithColor(size: Int, color: Color): Image =
    Image.circle(size).strokeColor(color)

  def concentricCirclesWithColor(count: Int, size: Int, color: Color): Image =
    count match
      case 0 => Image.empty
      case n => aCircleWithColor(size, color).on(concentricCirclesWithColor(n-1, size + 10, color.spin(15.degrees)))

  ////////////////////////////////////////////////////
  def chessboardRefactor(count: Int): Image =
    def square(color: Color): Image =
      Image.square(30).fillColor(color)

    def blockOfFour(): Image =
      square(Color.red).beside(square(Color.black)).above(square(Color.black).beside(square(Color.red)))

    def loop(count: Int): Image =
      count match {
        case 0 => blockOfFour()
        case n =>
          val unit = loop(n - 1)
          unit.beside(unit).above(unit.beside(unit))
      }

    loop(count)

  ////////////////////////////////////////////////
  def boxes(count: Int): Image =
    val aBox = Image.square(20).fillColor(Color.royalBlue)

    def loop(count: Int): Image =
      count match {
        case 0 => Image.empty
        case n => aBox.beside(loop(n - 1))
      }

    loop(count)

  ////////////////////////////////////////////////
  def sierpinskiWithColor(count: Int, color: Color): Image =
    def triangle(color: Color): Image =
      Image.triangle(20, 20).strokeColor(color).strokeWidth(5)

    def group(color: Color): Image =
      triangle(color).above(triangle(color).beside(triangle(color)))

    def loop(count: Int, color: Color): Image =
      count match {
        case 0 => group(color)
        case n =>
          def unit(color: Color) = loop(n - 1, color)
          unit(color.spin(30.degrees)).above(unit(color).beside(unit(color.spin(-30.degrees))))
      }

    loop(count, color)
}
