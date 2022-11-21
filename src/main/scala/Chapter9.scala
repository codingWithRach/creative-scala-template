import doodle.core.*
import doodle.image.*
import doodle.image.syntax.all.*
import doodle.image.syntax.core.*
import doodle.java2d.*

object Chapter9 {
  val square = (num: Int) => num * num
  val spinColor = (color: Color) => color.spin(15.degrees)
  val rotateImage = (image: Image) => image
    .beside(image.rotate(90.degrees))
    .beside(image.rotate(180.degrees))
    .beside(image.rotate(270.degrees))

  // type is Angle => Point
  // i.e. transforms an angle to a point
  val roseFn = (angle: Angle) =>
    Point.cartesian((angle * 7).cos * angle.cos, (angle * 7).cos * angle.sin)

  /////////////////////////////
  val c = Image.circle(40)
  val c1 = c.beside(c.at(10, 10)).beside(c.at(10, -10)).debug
  val c2 = c.debug.beside(c.at(10, 10).debug).beside(c.at(10, -10).debug)
  val c3 = c.debug.beside(c.debug.at(10, 10)).beside(c.debug.at(10, -10))
  val myImage = c1.above(c2).above(c3)

  /////////////////////////////////
  def parametricCircle(radius: Int, angle: Angle): Point =
    Point.polar(radius, angle)

  def sampleTheParametricCircle(radius: Int, samples: Int): Image = {
    // Angle.one is one complete turn i.e. 360 degrees
    val step = Angle.one / samples
    val dot = Image
      .triangle(10, 10)
      .fillColor(Color.limeGreen)
      .strokeColor(Color.lawngreen)

    def loop(count: Int): Image = {
      val angle = step * count
      count match {
        case 0 => Image.empty
        case n =>
          dot.at(parametricCircle(radius, angle)).on(loop(n - 1))
      }
    }

    loop(samples)
  }
  val myCircle = sampleTheParametricCircle(200, 60)

  //////////////////////////
  def genericSample(samples: Int, dot: Image, config: Int, curve: (Int, Angle) => Point): Image = {
    val step = Angle.one / samples

    def loop(count: Int): Image = {
      val angle = step * count
      count match {
        case 0 => Image.empty
        case n =>
          dot.at(curve(config, angle)).on(loop(n - 1))
      }
    }

    loop(samples)
  }

  def triangleDot(color: Color) = Image
    .triangle(10, 10)
    .fillColor(color)
    .strokeColor(color)
  val sampleGenericCircle = genericSample(60, triangleDot(Color.limeGreen), 200, parametricCircle)

  ///////////////////////
  def parametricSpiral(turns: Int, angle: Angle): Point = {
    Point((Math.exp(angle.toTurns) - turns) * 100, angle)
  }

  val smallCircle = genericSample(60, triangleDot(Color.limeGreen), 140, parametricCircle)
  val largeCircle = genericSample(60, triangleDot(Color.purple), 300, parametricCircle)
  val spiralPurple = genericSample(60, triangleDot(Color.purple), 1, parametricSpiral)
  val spiralCyan = genericSample(60, triangleDot(Color.darkCyan), 1, parametricSpiral)
  val spiralTwoTurns = genericSample(60, triangleDot(Color.darkCyan), 2, parametricSpiral)
  val exercise96102 =
    smallCircle
      .on(spiralPurple)
      .on(spiralPurple.transform(Transform.rotate(180.degrees)))
      .on(spiralCyan.transform(Transform.rotate(90.degrees)))
      .on(spiralCyan.transform(Transform.rotate(270.degrees)))

  ///////////////////////
  val rose = (petals: Int, angle: Angle) =>
    Point((angle * petals).cos * 200, angle)
  val plotRose7 = genericSample(600, triangleDot(Color.deepPink), 7, rose)
  val plotRose10 = genericSample(600, triangleDot(Color.deepPink), 10, rose)

  /////////////////////////
  def myDot(color: Color) = Image
    .circle(5)
    .fillColor(color)
    .strokeColor(color)

  def lissajous(a: Int, b: Int, offset: Angle): (Int, Angle) => Point =
    (config: Int, angle: Angle) =>
      Point(100 * ((angle * a) + offset).sin, 100 * (angle * b).sin)
  val plotLissajous11 = genericSample(100, myDot(Color.darkRed), 100, lissajous(1, 1, 90.degrees))
  val plotLissajous12 = genericSample(100, myDot(Color.darkRed), 100, lissajous(1, 2, 90.degrees))
  val plotLissajous13 = genericSample(100, myDot(Color.darkRed), 100, lissajous(1, 3, 90.degrees))
  val plotLissajous21 = genericSample(100, myDot(Color.darkCyan), 100, lissajous(2, 1, 90.degrees))
  val plotLissajous22 = genericSample(100, myDot(Color.darkCyan), 100, lissajous(2, 2, 90.degrees))
  val plotLissajous23 = genericSample(100, myDot(Color.darkCyan), 100, lissajous(2, 3, 90.degrees))
  val plotLissajous31 = genericSample(100, myDot(Color.purple), 100, lissajous(3, 1, 90.degrees))
  val plotLissajous32 = genericSample(100, myDot(Color.purple), 100, lissajous(3, 2, 90.degrees))
  val plotLissajous33 = genericSample(100, myDot(Color.purple), 100, lissajous(3, 3, 90.degrees))
  val plotLissajous = plotLissajous11.beside(plotLissajous12).beside(plotLissajous13)
    .above(plotLissajous21.beside(plotLissajous22).beside(plotLissajous23))
    .above(plotLissajous31.beside(plotLissajous32).beside(plotLissajous33))

  /////////////////////////
  def epicycloid(a: Int, b: Int, c: Int): (Int, Angle) => Point =
    (config: Int, angle: Angle) =>
      (Point(75, angle * a).toVec + Point(32, angle * b).toVec + Point(15, angle * c).toVec).toPoint
  val plotEpicycloid1 = genericSample(500, myDot(Color.green), 100, epicycloid(1, 6, 14))
  val plotEpicycloid2 = genericSample(800, myDot(Color.green), 100, epicycloid(7, 13, 25))
  val plotEpicycloid3 = genericSample(600, myDot(Color.green), 100, epicycloid(1, 7, -21))
  val plotEpicycloid = plotEpicycloid1.beside(plotEpicycloid2).beside(plotEpicycloid3)

  //////////////////////////////////////////////////////////////
  def newGenericSample(samples: Int, dot: Image, radius: Int, curve: (Int, Angle) => Point): Image = {
    val step = Angle.one / samples
    def loop(count: Int): Image = {
      val angle = step * count
      count match {
        case 0 => Image.empty
        case n =>
          dot.at(curve(radius, angle)).on(loop(n - 1))
      }
    }
    loop(samples)
  }

  def squareDot(color: Color) = Image
    .square(5)
    .fillColor(color)
    .strokeColor(color)

  def doubleCircleDot(color: Color) = Image
    .circle(10).fillColor(color)
    .on(Image.circle(20))
    .strokeColor(color)

  val rose5 = (radius: Int, angle: Angle) =>
    Point((angle * 5).cos * radius, angle)

  val plotSquareRose = newGenericSample(400, squareDot(Color.greenYellow), 250, rose5)
  val plotCircleRose = newGenericSample(600, doubleCircleDot(Color.fuchsia), 350, rose5)
    .transform(Transform.rotate(36.degrees))
  val plotDoubleRose = plotSquareRose.on(plotCircleRose)

  /////////////////////////////////////////////////////////////////
  val dropShadow = (image: Image) =>
    image.on(image.strokeColor(Color.black).fillColor(Color.black).at(5, -5))

  val mirrored = (image: Image) =>
    image.beside(image.transform(Transform.horizontalReflection))

  val composed = mirrored.andThen(dropShadow)

  val star = Image
    .star(5, 100, 30)
    .fillColor(Color.fireBrick)
    .strokeColor(Color.dodgerBlue)
    .strokeWidth(7.0)

  val starImages = dropShadow(star)
    .beside(mirrored(star))
    .beside(composed(star))
}
