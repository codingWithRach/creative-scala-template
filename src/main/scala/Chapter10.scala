import doodle.core.*
import doodle.image.*
import doodle.image.syntax.*
import doodle.image.syntax.core.*
import doodle.java2d.*
import doodle.core.PathElement.*

object Chapter10 {
  ///////////////////////////////////////////////////
  val triangle =
    List(
      lineTo(Point(50, 100)),
      lineTo(Point(100, 0)),
      lineTo(Point(0, 0))
    )
  val curve =
    List(curveTo(Point(50, 100), Point(100, 100), Point(150, 0)))
  def style(image: Image): Image =
    image.strokeWidth(6.0)
      .strokeColor(Color.royalBlue)
      .fillColor(Color.skyBlue)
  val openPaths = style(Image.openPath(triangle).beside(Image.openPath(curve)))
  val closedPaths = style(Image.closedPath(triangle).beside(Image.closedPath(curve)))
  val paths = openPaths.above(closedPaths)

  ///////////////////////////////////////////////
  // polygons example
  val myTriangle = List(
    moveTo(Point(0, 0)), lineTo(Point(0, 100)), lineTo(Point(86,50))
  )
  val mySquare = List(
    moveTo(Point(0, 0)), lineTo(Point(0, 100)), lineTo(Point(100, 100)), lineTo(Point(100, 0))
  )
  def myStyle(image: Image): Image =
    image.strokeWidth(6)
      .strokeColor(Color.mediumPurple)
      .fillColor(Color.cyan)
  def myStyledTriangle = myStyle(Image.closedPath(myTriangle))
  def myStyledSquare = myStyle(Image.closedPath(mySquare))
  def myShape = myStyledTriangle.beside(myStyledSquare)

  ////////////////////////////////////////
  // Building lists
  def ones(n: Int): List[Int] =
    n match {
      case 0 => Nil
      case n => 1 :: ones(n-1)
    }

  def descending(n: Int): List[Int] =
    n match {
      case 0 => Nil
      case n => n :: descending(n - 1)
    }

  // WHY ISN'T THIS VALID SYNTAX? *****************************
  // The recursive call in the case n expression always has to be last
  // putting anything after the recursive call causes an error
//  def myAscending(n: Int): List[Int] =
//    n match {
//      case 0 => Nil
//      case n => myAscending(n - 1) :: n :: Nil
//    }

  def ascending(n: Int): List[Int] =
    def iter(n: Int, counter: Int): List[Int] =
      n match {
        case 0 => Nil
        case n => counter :: iter(n-1, counter + 1)
      }
    iter(n, 1)

  def fill[A](n: Int, elem: A): List[A] =
    n match {
      case 0 => Nil
      case n => elem :: fill(n-1, elem)
    }

  //////////////////////////////////////////////////////////
  // Transforming lists
  def double(list: List[Int]): List[Int] =
    list match {
      case Nil => Nil
      case hd :: tl => hd * 2 :: double(tl)
    }

  def product(list: List[Int]): Int =
    list match {
      case Nil => 1
      case hd :: tl => hd * product(tl)
    }

  def contains[A](list: List[A], elem: A): Boolean =
    list match {
      case Nil => false
      case hd :: tl => (hd == elem) || contains(tl, elem)
    }

  def first[A](list: List[A], elem: A): A =
    list match {
      case Nil => elem
      case hd :: tl => hd
    }

  def reverse[A](list: List[A]): List[A] =
    def iter(list: List[A], reversed: List[A]): List[A] =
      list match {
        case Nil => reversed
        case hd :: tl => iter(tl, hd :: reversed)
      }
    iter(list, Nil)

  //////////////////////////////////////////////////////
  // replacing with map
  // note that cannot implement map unless the required output is a list
  // if the input is a list, then needs to have the same number of elements
  def mapOnes(n: Int): List[Int] =
    (0 until n).map(x => 1).toList

  def mapDescending(n: Int): List[Int] =
    (0 until n).map(x => n - x).toList
    // (0 until n by -1).toList

  def mapAscending(n: Int): List[Int] =
    (0 until n).map(x => x + 1).toList

  def mapDouble(list: List[Int]): List[Int] =
    list.map(n => 2 * n)

  // until gives a half-open interval
  // 1 until 5 => Range(1, 2, 3, 4)

  // to gives an open interval
  // 1 to 5 => Range(1, 2, 3, 4, 5)
  def openAscending(n: Int): List[Int] =
    (1 to n).toList

  /////////////////////////////////////////////////////
  // 10.4 stars
  def star(sides: Int, skip: Int, radius: Double): Image = {
    import Point._
    import PathElement._
    val rotation = 360.degrees * skip / sides
    val start = moveTo(polar(radius, 0.degrees))
    val elements = (1 until sides).toList map { index =>
      val point = polar(radius, rotation * index)
      lineTo(point)
    }
    Image.closedPath(start :: elements) strokeWidth 2
  }

  def allBeside(images: List[Image]): Image =
    images match {
      case Nil => Image.empty
      case hd :: tl => hd.beside(allBeside(tl))
    }

  def rowOfStars(sides: Int, radius: Int) =
    allBeside((1 to sides/2).toList.map(skip => star(sides, skip, radius)))

  def allAbove(images: List[Image]): Image =
    images match {
      case Nil => Image.empty
      case hd :: tl => hd.above(allAbove(tl))
    }

  def pyramidOfStars(n: Int) =
    allAbove((1 to n by 2).toList.map(x => rowOfStars(x, 20)))
}
