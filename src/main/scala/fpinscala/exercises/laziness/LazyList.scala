package fpinscala.exercises.laziness

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  // Natural
  def toListNatural: List[A] = this match {
    case Empty => Nil: List[A]
    case Cons(h,t) => h()::t().toList
  }
  // Tail Recursive
  def toList: List[A] = {
    @annotation.tailrec
    def loop(as: LazyList[A], res: List[A]): List[A] = as match {
      case Empty => res
      case Cons(h,t) => loop(t(), h()::res)
    }
    loop(this, Nil: List[A]).reverse
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = this match {
    case _ if n <= 0 => LazyList.empty 
    case Cons(h, t) => Cons(h, () => t().take(n-1))
    case _ => LazyList.empty
  }

  def drop(n: Int): LazyList[A] = this match {
    case Cons(_,t) if n >= 1 => t().drop(n-1)
    case x => x
  }

  def takeWhile(p: A => Boolean): LazyList[A] = this match {
    case Cons(h,t) if (p(h())) => LazyList.cons(h(), t().takeWhile(p))
    case _ => LazyList.empty
  }

  def forAll(p: A => Boolean): Boolean = 
    foldRight(true)((a,b) => p(a) && b)

  def takeWhileViaFoldRight(p: A => Boolean): LazyList[A] = 
    foldRight(Empty: LazyList[A])((a, as) => if (p(a)) then LazyList.cons(a,as) else Empty: LazyList[A])

  def headOption: Option[A] = 
    foldRight(None:Option[A])((a,_) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): LazyList[B] = 
    foldRight(Empty:LazyList[B])((a, bs) => LazyList.cons(f(a), bs))

  def filter(f: A => Boolean): LazyList[A] =
    foldRight(Empty:LazyList[A])((a,as) => if (f(a)) then LazyList.cons(a,as) else as)

  def append[A2>:A](list: => LazyList[A2]): LazyList[A2] =
    foldRight(list)((a,acc) => LazyList.cons(a,acc))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(Empty:LazyList[B])((a, acc) => f(a).append(acc))

  def mapViaUnfold[B](f: A => B): LazyList[B] = 
    LazyList.unfold(this)(as => as match {
      case Empty => None
      case Cons(h,t) => Some((f(h()), t()))
    })
  
  def takeViaUnfold(n: Int): LazyList[A] =
    LazyList.unfold((this,n):Tuple2[LazyList[A],Int]){
      case (Cons(h,t),i) if i > 0 => Some((h(), (t(),i-1)))
      case _ => None
    }

  def takeWhileViaUnfold(f: A => Boolean): LazyList[A] =
    LazyList.unfold(this){
      case Cons(h,t) if f(h()) => Some((h(),t()))
      case _ => None
    }

  def zipWith[B,C](bs: LazyList[B])(f: (A,B) => C): LazyList[C] = 
    LazyList.unfold((this,bs)){
      case (Empty,_) => None
      case (_,Empty) => None
      case (Cons(ah,at),Cons(bh,bt)) => Some((f(ah(),bh()), (at(),bt())))
    }

  def zipAll[B](s2: LazyList[B]): LazyList[(Option[A],Option[B])] = 
    LazyList.unfold((this,s2)){
      case (Empty,Empty) => None
      case (as,bs) => Some((as.headOption,bs.headOption), (as.drop(1),bs.drop(1)))
    }

  // I kinda cheated lol. 
  def startsWith[B](s: LazyList[B]): Boolean = 
    this.zipAll(s).takeWhile{case (_,x) => x match {
      case(Some(_)) => true 
      case _ => false
    }}.forAll{
      case (None,_) => false
      case (Some(a),Some(b)) => a == b
      case _ => false
    }
  
  def tails: LazyList[LazyList[A]] = 
    LazyList.unfold[LazyList[A], LazyList[A]](this){
      case Empty => None
      case Cons(h,t) => Some((Cons(h,t),t()))
    }.append(LazyList.cons(Empty:LazyList[A], Empty:LazyList[LazyList[A]]))

  def hasSubsequence[A](s: LazyList[A]): Boolean =
    tails.exists(_.startsWith(s))

  def scanRight[B](z: B)(f: (A, => B) => B): LazyList[B] = 
    foldRight(LazyList(z))((a,acc) => LazyList.cons(f(a,acc.headOption.get),acc))

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = 
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty 
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] = LazyList.cons(a, continually(a))

  def from(n: Int): LazyList[Int] = LazyList.cons(n, from(n+1))

  lazy val fibs: LazyList[Int] = {
    def loop(f0: Int, f1: Int): LazyList[Int] = {
      LazyList.cons(f0, loop(f1,f0+f1))
    }
    loop(0,1)
  }

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = f(state) match {
    case None => Empty:LazyList[A]
    case Some((a,s)) => LazyList.cons(a, unfold(s)(f))
  }

  lazy val fibsViaUnfold: LazyList[Int] = 
    unfold((0,1):Tuple2[Int,Int])((s) => Some((s._1,(s._2,s._1+s._2))))

  def fromViaUnfold(n: Int): LazyList[Int] = 
    unfold(n)((x) => Some(x,x+1))

  def continuallyViaUnfold[A](a: A): LazyList[A] = 
    unfold(a)(x => Some(x,x))

  lazy val onesViaUnfold: LazyList[Int] = 
    unfold(1)(_ => Some(1,1))
