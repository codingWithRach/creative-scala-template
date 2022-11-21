import doodle.core.*
import doodle.image.*
import doodle.image.syntax.all.*
import doodle.image.syntax.core.*
import doodle.java2d.*

object Chapter9Cont {
  // 9.8 Higher Order Methods and Functions ******************
  val parametricCircle: Angle => Point =
    (angle: Angle) => Point(1.0, angle)

  def scale(factor: Double): Point => Point =
    (point: Point) => Point(point.r * factor, point.angle)

  val circle100 = parametricCircle.andThen(scale(100))
  val circle200 = parametricCircle.andThen(scale(200))
  val circle300 = parametricCircle.andThen(scale(300))

  val growingDot: Point => Image =
    (pt: Point) => Image.circle(pt.angle.toTurns * 20).at(pt)

  val growingCircle = parametricCircle
    .andThen(scale(100))
    .andThen(growingDot)

  def sample(samples: Int, curve: Angle => Image): Image = {
    val step = Angle.one / samples

    def loop(count: Int): Image = {
      val angle = step * count
      count match {
        case 0 => Image.empty
        case n =>
          curve(angle).on(loop(n - 1))
      }
    }
    loop(samples)
  }

  val plotGrowingCircle = sample(100, growingCircle)

  // repeat for spiral
  val parametricSpiral: Angle => Point =
    (angle: Angle) => Point(Math.exp(angle.toTurns - 1), angle)

  val spiral100 = parametricSpiral.andThen(scale(100))
  val spiral200 = parametricSpiral.andThen(scale(200))
  val spiral300 = parametricSpiral.andThen(scale(300))

  val growingSpiral = parametricSpiral
    .andThen(scale(100))
    .andThen(growingDot)

  val plotGrowingSpiral = sample(100, growingSpiral)

  // 9.8.1 More uses of composition ****************************
  def concentricShapes(count: Int, singleShape: Int => Image): Image = count match {
    case 0 => Image.empty
    case n => singleShape(n).on(concentricShapes(n - 1, singleShape))
  }

  // Passing a function literal directly:
  val blackCircles: Image =
    concentricShapes(10, (n: Int) => Image.circle(50 + 5 * n))

  // Converting a method to a function:
  def redCircle(n: Int): Image =
    Image.circle(50 + 5*n).strokeColor(Color.red)
  val redCircles: Image =
    concentricShapes(10, redCircle _)

  // Exercise for image 9.14 ******************************
  def outlinedCircle(n: Int): Image =
    Image.circle((n * 20)+50).strokeWidth(8)
  val concentricCircles = concentricShapes(10, outlinedCircle _)

  def outlinedTriangle(n: Int): Image =
    Image.triangle((n*20)+50, (n*20)+50).strokeWidth(8)
  val concentricTriangles = concentricShapes(10, outlinedTriangle _)

  def outlinedRectangle(n: Int): Image =
    Image.rectangle((n*20)+50, (n*20)+50).strokeWidth(8)
  val concentricRectangles = concentricShapes(10, outlinedRectangle _)

  // without colour
  val combinedBlackShapes = concentricCircles.beside(concentricTriangles).beside(concentricRectangles)

  // aside
  def circleOrSquare(n: Int) =
    if (n % 2 == 0) Image.rectangle(n * 20, n * 20) else Image.circle(n * 20)

  val plotCircleOrSquare = concentricShapes(10, outlinedCircle)
    .beside(concentricShapes(10, circleOrSquare))

  // add colour
  def colored(shape: Int => Image, color: Int => Color): Int => Image = (n: Int) =>
    shape(n).strokeWidth(8).strokeColor(color(n))

  def fading(n: Int): Color =
    Color.blue.fadeOut((1 - n / 20.0).normalized)

  def spinning(n: Int): Color =
    Color.blue.desaturate(0.5.normalized).spin((n * 30).degrees)

  def size(n: Int): Double =
    (20 * n) + 50
  def circle(n: Int): Image =
    Image.circle(size(n))
  def square(n: Int): Image =
    Image.square(size(n))
  def triangle(n: Int): Image =
    Image.triangle(size(n), size(n))

  val combinedShapesWithColour =
    concentricShapes(10, colored(circle, spinning))
      .beside(concentricShapes(10, colored(triangle, fading))
          .beside(concentricShapes(10, colored(square, spinning))))

  // Fig 9.17
  def dottyCircle(n: Int): Image =
    sample(72,
      parametricCircle
        .andThen(scale(100 + n * 24))
        .andThen(growingDot))

  val plotDottyCircle = concentricShapes(10, colored(dottyCircle, spinning))

}
