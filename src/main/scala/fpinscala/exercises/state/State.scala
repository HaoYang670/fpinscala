package fpinscala.exercises.state


trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (n, new_rng) = rng.nextInt
    if n >= 0
    then (n, new_rng)
    else if n > Int.MinValue
    then (-n, new_rng)
    else nonNegativeInt(rng)

  def double(rng: RNG): (Double, RNG) =
    map(nonNegativeInt){n =>
      n / (Int.MaxValue.toDouble + 1)
    }(rng)

  def intDouble(rng: RNG): ((Int,Double), RNG) =
    val (i, rng0) = rng.nextInt
    val (d, rng1) = double(rng0)
    ((i, d), rng1)

  def doubleInt(rng: RNG): ((Double,Int), RNG) =
    val ((i, d), rng0) = intDouble(rng)
    ((d, i), rng0)

  def double3(rng: RNG): ((Double,Double,Double), RNG) =
    val (d0, rng0) = double(rng)
    val (d1, rng1) = double(rng0)
    val (d2, rng2) = double(rng1)
    ((d0, d1, d2), rng2)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill(count)(int))(rng)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng0) = ra(rng)
      val (b, rng1) = rb(rng0)
      (f(a, b), rng1)
    }

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      rs match
        case Nil => (Nil, rng)
        case hr::tr =>
          val (tl, rng0) = sequence(tr)(rng)
          val (hd, rng1) = hr(rng0)
          (hd::tl, rng1)
    }

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng0) = r(rng)
      f(a)(rng0)
    }

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)(a => rng => (f(a), rng))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => rng => {
      val (b, rng0) = rb(rng)
      (f(a, b), rng0)
    })

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =
      s => {
        val (a, s0) = run(s)
        (f(a), s0)
      }

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      s => {
        val (a, s0) = run(s)
        val (b, s1) = sb(s0)
        (f(a, b), s1)
      }

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s => {
        val (a, s0) = run(s)
        f(a)(s0)
      }

  def sequence[S, A](rs: List[A])(f: A => State[S, S]): State[S, S] =
    s => {
      rs.foldLeft((s, s))((st, b) =>
        f(b)(st._2)
      )
  }

  def apply[S, A](f: S => (A, S)): State[S, A] = f

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    State.sequence(inputs)((inp: Input) => {
      State((st: Machine) => {
        val new_st = (inp, st) match
          case (_, Machine(_, 0, _)) => st
          case (Input.Coin, Machine(false, _, _)) |
               (Input.Turn, Machine(true, _, _)) => st
          case (Input.Coin, Machine(true, candies, coins)) => Machine(false, candies, coins + 1)
          case (Input.Turn, Machine(false, candies, coins)) => Machine(true, candies - 1, coins)
        (new_st, new_st)
      })
    }).map(m => (m.coins, m.candies))
