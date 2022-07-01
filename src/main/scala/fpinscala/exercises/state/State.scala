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
    val (n, rng2) = rng.nextInt
    if (n<0) (~n, rng2)
    else (n,rng2)

  def double(rng: RNG): (Double, RNG) = 
    val (n, rng2) = nonNegativeInt(rng)
    (((n-1).abs).toDouble/Int.MaxValue.toDouble, rng2) // lol this solution is terrible

  def intDouble(rng: RNG): ((Int,Double), RNG) = 
    ((rng.nextInt._1, double(rng)._1), rng.nextInt._2)

  def doubleInt(rng: RNG): ((Double,Int), RNG) = 
    ((double(rng)._1,rng.nextInt._1),rng.nextInt._2)

  def double3(rng: RNG): ((Double,Double,Double), RNG) = 
    val (d1,rng2) = double(rng)
    val (d2,rng3) = double(rng2)
    val (d3,rng4) = double(rng3)
    ((d1,d2,d3),rng4)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def loop(n: Int, rng: RNG, res: (List[Int],RNG)): (List[Int],RNG) = {
      if (n <= 0) res
      else {
        val (i, rng2) = rng.nextInt
        loop(n-1,rng2,(i::res._1,rng2))
      }
    }

    loop(count,rng,(Nil:List[Int],rng))
  }

  def doubleViaMap(rng: RNG): (Double, RNG) = 
    map(nonNegativeInt)(x => x.toDouble/(Int.MaxValue.toDouble+1.0))(rng)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => 
    val (a,r2) = ra(rng)
    val (b,r3) = rb(r2)
    (f(a,b),r3)

  // Did this without cheating. 
  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] = 
    rs.foldRight(unit(Nil:List[A]))((ra,ras) => map2(ra,ras)((a,as) => a::as))

  def intsViaSequence(count: Int)(rng: RNG): (List[Int], RNG) = 
    sequence(List.fill(count)(int))(rng)

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] = 
    r0 => {
      val (a,r1) = r(r0)
      f(a)(r1)
    }

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] = 
    flatMap(r)(a => unit(f(a))) // damn, nice

    // Exercise 6.9 was so hard for me lmao
  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    flatMap(rb)(b => flatMap(ra)(a => unit(f(a,b)))) // Idek how this works

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] = s0 =>
      val (a,s1) = run(s0)
      (f(a),s1)

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = s0 => 
      val (a,s1) = run(s0)
      val (b,s2) = sb.run(s1)
      (f(a,b),s2)

    def flatMap[B](f: A => State[S, B]): State[S, B] = s0 => 
      val (a,s1) = run(s0)
      f(a)(s1)

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def unit[S, A](a: A): State[S,A] = s => (a,s)

  def sequence[S, A](ss: List[State[S,A]]): State[S,List[A]] = 
    ss.foldRight(unit(Nil:List[A]):State[S,List[A]])((s,acc) => s.map2(acc)((a,as) => a::as))
    
enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State((m:Machine) =>
    val rm = inputs.foldLeft(m)((nm, input) => 
      val (locked,candies,coins) = (nm.locked, nm.candies, nm.coins)
      input match {
        case _ if candies <= 0 => nm
        case Input.Coin if locked => Machine(false, candies, coins+1)
        case Input.Turn if !locked => Machine(true,candies-1,coins)
        case _ => nm
      }
    )
    ((rm.coins, rm.candies),rm)
  )
