package fpinscala.exercises.datastructures

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(ds: List[Double]): Double = ds match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val x = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x // 3 doesn't match
    case Nil => 42 // Not Nil for sure
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // 1+2=3. Seems right
    case Cons(h, t) => h + sum(t) // Also matches, and would yield 1+sum(2,3,4,5)=15, but already matched so doesn't apply
    case _ => 101 // Already matched above, so doesn't apply

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]) =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]) =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // Exercise 3.2
  def tail[A](l: List[A]): List[A] = l match{
    case Nil => sys.error("Empty List")
    case Cons(_, t) => t
  }

  // Exercise 3.3
  def setHead[A](l: List[A], h: A): List[A] = l match{
    case Nil => sys.error("Empty List")
    case Cons(_,t) => Cons(h,t)
  }

  // Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def loop(curr: List[A], n: Int): List[A] = {
      // I mean, whatever, I guess just use normal recursion rather than this weird loop recursion
      if (n<=0) curr
      else curr match{
        case Nil => Nil
        case Cons(_,t) => loop(t,n-1)
      }
    }

    loop(l,n)
  }

  // Exercise 3.5
  @annotation.tailrec 
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h,t) => {
      if (f(h)) dropWhile(t,f)
      else l
    }
    // bro ok offical solution be like:. How tf were we suposed to know that?  
    case Cons(h,t) if (f(h)) => dropWhile(t,f)
    case _ => l
  }

  // Exercise 3.6. I swear to god, I didn't even check teh answer and Its basically correct byte for byte
  def init[A](l: List[A]): List[A] = l match{
    case Nil => sys.error("Empty List") 
    case Cons(_,Nil) => Nil
    case Cons(h,t) => Cons(h,init(t))
  }

  /* --- Exercise 3.7 ---
    Short-cuircuting will not occur, because the match experssion in foldRight will continue to operate
    To use short-cuircuting, do a check for any x where x applied to any other value is x

    --- Exercise 3.8 ---
    Wouldn't you just get the List(1,2,3) back? You do indeed. 
  */

  // Exercise 3.9
  def length[A](l: List[A]): Int = foldRight(l, 0,(_,x)=>(x+1))

  // Exercise 3.10 #I'm cracked. 
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B = l match {
    case Nil => acc
    case Cons(h,t) => foldLeft(t,f(acc,h),f)
  }

  // Exercise 3.11 for next 3 functions
  def sumViaFoldLeft(ns: List[Int]) = foldLeft(ns, 0, _+_)

  def productViaFoldLeft(ns: List[Double]) = foldLeft(ns, 1.0, _*_)

  def lengthViaFoldLeft[A](l: List[A]): Int = foldLeft(l, 0, (acc,_) => acc+1)

  // Exercise 3.12
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A], (b,a)=>Cons(a,b))

  // Exercise 3.13
  def foldRightViaFoldLeft[A,B](l: List[A], acc: B, f: (A,B) => B): B = 
    foldLeft(reverse(l), acc, (b: B, a: A) => f(a,b))

  // Exercise 3.14
  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = foldRight(l, r, Cons(_, _))
  // How tf does appendViaFoldLeft work? Fuck it, i don't really care

  // Exercise 3.15
  def concat[A](l: List[List[A]]): List[A] = 
    // appendViaFoldRight(larr, rarr) is linear on larr, so concat is linear
    foldRight(l, Nil:List[A], appendViaFoldRight(_,_)) // Could just use append

  // Exercise 3.16
  def incrementEach(l: List[Int]): List[Int] = 
    foldRight(l, Nil: List[Int], (a,b) => Cons(a+1, b))

  // Exercise 3.17
  def doubleToString(l: List[Double]): List[String] = 
    foldRight(l, Nil:List[String], (a,b) => Cons(a.toString, b))

  // Exercise 3.18
  def map[A,B](l: List[A])(f: A => B): List[B] = 
    foldRight(l, Nil:List[B], (a,acc) => Cons(f(a), acc))

  // Exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = 
    foldRight(as, Nil: List[A], (a,acc) => 
      if (f(a)) Cons(a,acc)
      else acc
    )

  // Exercise 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = 
    concat(map(as)(f))

  // Exercise 3.21
  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = 
    flatMap(as)((a) => if (f(a)) then List(a) else List())

  // Exercise 3.22
  def addPairwise(a: List[Int], b: List[Int]): List[Int] = {
    @annotation.tailrec
    def loop(a: List[Int], b: List[Int], acc: List[Int]): List[Int] = a match {
      case Nil => acc 
      case Cons(ah,at) => b match {
        case Nil => acc
        case Cons(bh,bt) => loop(at, bt, Cons(ah + bh, acc))
      }
    }

    reverse(loop(a,b,Nil: List[Int]))
  }

  // Exercise 3.23. Also show us some syntax sugar in the book, brother
  def zipWithNormalRecurse[A,B,C](as: List[A], bs: List[B], f: (A,B) => C): List[C] = (as,bs) match{
    case (Nil,_) => Nil
    case (_,Nil) => Nil
    case (Cons(ah,at),Cons(bh,bt)) => Cons(f(ah,bh),zipWith(at,bt,f)) // Not tail recursive
  } 

  // ZipWith with Tail Recursion. This was pretty fun actually
  def zipWith[A,B,C](as: List[A], bs: List[B], f: (A,B) => C): List[C] = {
    @annotation.tailrec
    def loop(as: List[A], bs: List[B], res: List[C]): List[C] = (as,bs) match {
      case (Nil,_) => res 
      case (_,Nil) => res 
      case (Cons(ah,at),Cons(bh,bt)) => loop(at, bt, Cons(f(ah,bh), res)) // Tail Recursive
    }

    reverse(loop(as, bs, Nil: List[C]))
  } 

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup,sub) match {
    case (_,Nil) => true
    case (Nil,_) => false
    case (Cons(suph,supt),Cons(subh,subt)) => {
      if (suph == subh) hasSubsequence(supt,subt)
      else hasSubsequence(supt,sub)
    }
  }
