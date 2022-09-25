import doodle.core.*
import doodle.image.*
import doodle.image.syntax.all.*
import doodle.image.syntax.core.*
import doodle.java2d.*
import cats.effect.unsafe.implicits.global

object Streets {
  val getRectangle: (Int, Int, Color) => Image = {
    (width: Int, height: Int, color: Color) =>
      Image.rectangle(width, height).fillColor(color).noStroke
  }

  val roof: Image = Image.triangle(100, 60).fillColor(Color.darkRed).noStroke
  val walls: Image = getRectangle(100, 80, Color.red)
  val door: Image = getRectangle(20, 35, Color.red).above(getRectangle(20, 45, Color.black))
  val house: Image = roof.above(door.on(walls))

  val branches: Image = Image.circle(100).fillColor(Color.green)
  val trunk: Image = getRectangle(20, 40, Color.brown)
  val tree: Image = branches.above(trunk)

  val space: Image = getRectangle(40,140, Color.white)
  val houseAndTree: Image = space.beside(house).beside(tree).beside(space)

  val groundSection: Image = getRectangle(59, 10, Color.yellow).beside(getRectangle(34, 10, Color.black))
  val upperGround: Image = groundSection.beside(groundSection).beside(groundSection).beside(groundSection).beside(groundSection).beside(groundSection).beside(groundSection).beside(groundSection).beside(groundSection)
  val ground: Image = upperGround.above(getRectangle((59 + 34) * 9, 10, Color.black))

  val street: Image = houseAndTree.beside(houseAndTree).beside(houseAndTree).above(ground)

  def main(args: Array[String]): Unit = {
    street.draw()
  }
}
