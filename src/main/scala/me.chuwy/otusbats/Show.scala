package me.chuwy.otusbats


trait Show[A] {
  def show(a: A): String
}

object Show {

  // 1.1 Instances (`Int`, `String`, `Boolean`)
  def from[T](f: T => String): Show[T] = new Show[T] {
    override def show(a: T): String = f(a)
  }

  implicit val intShow: Show[Int] = from[Int](_.toString)
  implicit val stringShow: Show[String] = from[String](identity)
  implicit val booleanShow: Show[Boolean] = from[Boolean](_.toString)


  // 1.2 Instances with conditional implicit

  implicit def listShow[A](implicit ev: Show[A]): Show[List[A]] = new Show[List[A]] {
     def show(list: List[A]): String = list.mkString(" ")
  }


  // 2. Summoner (apply)
  def apply[A](implicit ev: Show[A]): Show[A] = ev


  // 3. Syntax extensions

  implicit class ShowOps[A](a: A) {
    def show(implicit ev: Show[A]): String = ev.show(a)

    def mkString_[B](begin: String, end: String, separator: String)(implicit S: Show[B], ev: A <:< List[B]): String = {
      // with `<:<` evidence `isInstanceOf` is safe!
      val casted: List[B] = a.asInstanceOf[List[B]]
      Show.mkString_(casted, separator, begin, end)
    }

  }

  /** Transform list of `A` into `String` with custom separator, beginning and ending.
   *  For example: "[a, b, c]" from `List("a", "b", "c")`
   *
   *  @param separator. ',' in above example
   *  @param begin. '[' in above example
   *  @param end. ']' in above example
   */
  def mkString_[A: Show](list: List[A], begin: String, end: String, separator: String): String =
    list.mkString(begin, separator, end)


  // 4. Helper constructors

  /** Just use JVM `toString` implementation, available on every object */
  def fromJvm[A]: Show[A] = new Show[A] {
    override def show(a: A): String = a.toString
  }
  
  /** Provide a custom function to avoid `new Show { ... }` machinery */
  def fromFunction[A](f: A => String): Show[A] = from[A](f)

}
