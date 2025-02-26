package fpinscala.exercises.errorhandling

// Hide std library `Option` since we are writing our own in this chapter
import scala.{Option as _, Some as _, None as _}

enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] =
    this match
      case Some(get) => Some(f(get))
      case None => None

  def getOrElse[B>:A](default: => B): B =
    this match
      case Some(get) => get
      case None => default

  def flatMap[B](f: A => Option[B]): Option[B] =
    this match
      case Some(get) => f(get)
      case None => None    

  def orElse[B>:A](ob: => Option[B]): Option[B] =
    this match
      case Some(_) => this
      case None => ob

  def filter(f: A => Boolean): Option[A] =
    this match
      case Some(get) =>
        if f(get)
        then this
        else None
      case None => None

object Option:

  def failingFn(i: Int): Int =
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try
      val x = 42 + 5
      x + y
    catch case e: Exception => 43 // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.

  def failingFn2(i: Int): Int =
    try
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    catch case e: Exception => 43

  def mean(xs: Seq[Double]): Option[Double] =
    if xs.isEmpty then None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    this.mean(xs)
      .flatMap(
        m => this.mean(
          xs.map(x => math.pow(x - m, 2))
        )
      )
    

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match
      case (None, _) | (_, None) => None
      case (Some(a), Some(b)) => Some(f(a, b))

  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    as match
      case Nil => Some(Nil)
      case hd :: tl =>
        hd.flatMap(hd =>
          sequence(tl).map(tl =>
            hd :: tl))

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as match
      case Nil => Some(Nil)
      case hd :: tl =>
        f(hd).flatMap(hd => 
          traverse(tl)(f).map(tl =>
            hd :: tl))
