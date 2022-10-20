import doodle.core.*
import doodle.image.*
import doodle.image.syntax.all.*
import doodle.image.syntax.core.*
import doodle.java2d.*

object Chapter7 {
  val aBox: Image = Image.square(20).fillColor(Color.royalBlue)

  def boxes(count: Int): Image =
    count match {
      case 0 => Image.empty
      case n => aBox.beside(boxes(n - 1))
    }

  def stackedBoxes(count: Int): Image =
    count match {
      case 0 => Image.empty
      case n => aBox.above(stackedBoxes(n - 1))
    }

  val aCircle: Image = Image
    .circle(60)
    .strokeColor(Color.midnightBlue)
    .strokeWidth(5.0)

  def alternatingRow(count: Int): Image =
    count match {
      case 0 => Image.empty
      case n =>
        n % 2 == 0 match {
          case true => aBox.beside(alternatingRow(n - 1))
          case false => aCircle.beside(alternatingRow(n - 1))
        }
    }

  def aTriangle(size: Int, color: Color): Image = Image
    .triangle(size, size)
    .strokeColor(color)
    .strokeWidth(5)

  def funRow(count: Int): Image =
    count match {
      case 0 => Image.empty
      case n => aTriangle(n * 50, Color.red.spin((n * 10).degrees)).beside(funRow(n - 1))
    }

  ////////////////////////

  def aSpace(size: Int): Image =
    Image.square(size).fillColor(Color.white).noStroke

  def aVerticalCross(count: Int): Image =
    count match {
      case 0 => Image.empty
      case n => aBox.above(aSpace(2)).above(aVerticalCross(n - 1))
    }

  def aHorizontalCross(count: Int): Image =
    count match {
      case 0 => Image.empty
      case n => aBox.beside(aSpace(2)).beside(aHorizontalCross(n - 1))
    }

  def aCross(count: Int): Image =
    aVerticalCross(count)
      .above(aSpace(2))
      .above(aHorizontalCross(count).beside(aTriangle(30, Color.red).beside(aHorizontalCross(count))))
      .above(aSpace(2))
      .above(aVerticalCross(count))

  def cross(count: Int): Image =
    count match {
      case 0 => Image.empty
      case n => aCross(n).beside(aSpace(30)).beside(cross(n - 1))
    }
}
