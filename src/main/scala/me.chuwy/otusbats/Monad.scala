package me.chuwy.otusbats

trait Monad[F[_]] extends Functor[F] { self =>
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def point[A](a: A): F[A]

  def flatten[A](fa: F[F[A]]): F[A] = flatMap(fa)(identity)
}

object Monad {

  def apply[F[_]](implicit ev: Monad[F[_]]): Monad[F[_]] = ev

  implicit val optionMonad: Monad[Option] = new Monad[Option] {
    def flatMap[A, B](option: Option[A])(f: A => Option[B]): Option[B] = option.flatMap(f)
    def point[A](a: A): Option[A] = Some(a)
    def map[A, B](option: Option[A])(f: A => B): Option[B] = option.map(f)
  }

  implicit val listMonad: Monad[List] = new Monad[List] {
    override def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = list.flatMap(f)
    override def point[A](a: A): List[A] = List(a)
    override def map[A, B](list: List[A])(f: A => B): List[B] = list.map(f)
  }

  implicit val mapMonad: Monad[Vector] = new Monad[Vector] {
    override def flatMap[A, B](fa: Vector[A])(f: A => Vector[B]): Vector[B] = fa.flatMap(f)
    override def point[A](a: A): Vector[A] = Vector(a)
    override def map[A, B](fa: Vector[A])(f: A => B): Vector[B] = fa.map(f)
  }

}
