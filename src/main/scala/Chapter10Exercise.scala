object Chapter10Exercise {
  def reverse[A](list: List[A]): List[A] =
    def iter(list: List[A], reversed: List[A]): List[A] =
      list match {
        case Nil => reversed
        case hd :: tl => iter(tl, hd :: reversed)
      }

    iter(list, Nil)

  def reverseString(s: String): String =
    reverse(s.toList).mkString

  def reverseListOfStrings(list: List[String]): List[String] =
    reverse(list.map(reverseString(_)))
}
