import doodle.core.*
import doodle.image.*
import doodle.image.syntax.all.*
import doodle.image.syntax.core.*
import doodle.java2d.*
import doodle.reactor.*

import scala.concurrent.duration.*
import cats.effect.unsafe.implicits.global

import scala.annotation.tailrec

// Histogram representation:
// - height of bar represents percentage of recipients receiving notification within specified time
// - darkness of bar represents number of recipients (the darker the bar the higher the number)
object Histogram {
  val recipients: List[Double] = List(1892051, 881381, 288286, 886214, 558151, 888289, 542379, 895883, 556439, 1908051)
  val percentages: List[Double] = List(63.28, 85.67, 95.31, 94.15, 94.29, 79.6, 90.88, 88.78, 73.66, 59.88)

  val heightFactor: Int = 4
  val heights: List[Int] = percentages.map(percentage => math.round(percentage).toInt * heightFactor)
  val darkenFactors: List[Normalized] = recipients.map(recipient => (recipient / 1000000 - 0.3).normalized)

  val maxHeight: Int = heights.max
  val colWidth: Int = 50
  val gapWidth: Int = 10
  val axisWidth: Int = 3

  val getRectangle: (Int, Int, Color) => Image = {
    (width: Int, height: Int, color: Color) =>
    val block: Image = Image.rectangle(width, height).fillColor(color).noStroke
      .beside(Image.rectangle(gapWidth, height).fillColor(Color.white).noStroke)
    if (height < maxHeight)
      Image.rectangle(width, maxHeight - height).fillColor(Color.white).noStroke.above(block)
    else
      block
  }

  val images: List[Image] = heights.zipWithIndex.map {
    case (blockHeight, index) =>
      getRectangle(colWidth, blockHeight, Color.lightBlue.darken(darkenFactors(index)))
  }

  @tailrec def getImage(existingImage: Image, images: List[Image], index: Int): Image = {
    if (index == images.length - 1) existingImage
    else getImage(existingImage.beside(images(index)), images, index + 1)
  }

  val histogram: Image = getImage(images.head, images, 1)

  val histogramWithAxes: Image = getRectangle( axisWidth, maxHeight, Color.black ).beside(histogram)
    .above( Image.rectangle( (heights.length - 1) * (colWidth + gapWidth) + gapWidth, axisWidth).fillColor(Color.black).noStroke)

  def main(args: Array[String]): Unit = {
    histogramWithAxes.draw()
  }
}
