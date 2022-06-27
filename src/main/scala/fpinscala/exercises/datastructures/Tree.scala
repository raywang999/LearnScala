package fpinscala.exercises.datastructures

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  // Why was this already implemented? 
  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int = this match {
    case Leaf(_) => 0
    case Branch(l,r) => 1 + (l.depth max r.depth)
  }

  def map[B](f: A => B): Tree[B] = this match {
    case Leaf(l) => Leaf(f(l)) 
    case Branch(l,r) => Branch(l.map(f),r.map(f))
  }

  def fold[B](f: A => B, g: (B,B) => B): B = this match {
    case Leaf(l) => f(l)
    case Branch(l,r) => g(l.fold(f,g),r.fold(f,g))
  }
  
  def sizeViaFold: Int = fold(_ => 1, (l,r) => 1 + l + r)
  
  def depthViaFold: Int = fold(_ => 0, (l,r) => 1 + (l max r))
  
  def mapViaFold[B](f: A => B): Tree[B] = fold(a => Leaf(f(a)), (l,r) => Branch(l,r))

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)

  extension (t: Tree[Int]) def firstPositive: Option[Int] = t match {
    case Leaf(x) => if (x>0) then Some(x) else None
    case Branch(l,r) => l.firstPositive orElse r.firstPositive
  }

  extension (t: Tree[Int]) def maximum: Int = t match {
    case Leaf(x) => x
    case Branch(l,r) => l.maximum max r.maximum
  }

  extension (t: Tree[Int]) def maximumViaFold: Int = t.fold(a => a, (l,r) => l max r)
