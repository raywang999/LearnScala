//package myfiles.datastructures

sealed trait MyList[+A]
case object MyNil extends MyList[Nothing]
case class MyCons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList { 
    def sum(ints: MyList[Int]): Int = ints match { // You're stupid. No I'm Not.
        case MyNil => 0
        case MyCons(9,MyCons(10,x)) => 21 // What's 9+10? 21
        case MyCons(x,xs) => x + sum(xs)
    }
    def product(ds: MyList[Double]): Double = ds match {
        case MyNil => 1.0
        case MyCons(x,xs) => x*product(xs)
    }
    def apply[A](as: A*): MyList[A] = {
        if (as.isEmpty) MyNil
        else MyCons(as.head, apply(as.tail*))
    }
    def foldRight[A,B](as: MyList[A], z: B)(f: (A,B) => B): B = as match{
        case MyNil => z
        case MyCons(h,t) => f(h,foldRight(t,z)(f)) 
    }
}
