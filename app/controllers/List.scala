package controllers

import play.api._
import play.api.mvc._


sealed trait List[+A]
case object Nil extends List[Nothing]

// a construct, to allow matching of Lists...
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List extends Controller {

  def index = Action {

    val ex3_1 = "The output from ex3.1 is " + List.x
    val ex3_1a = "The output to see which case matches next in ex3.1 is " + List.x2
    val ex3_4 = "The output to ex3.4 is " + List.x3_4
    val ex3_6 = "The output to ex3.4 is " + List.x3_6
    val ex3_9 = "The output to ex3.9 is " + List.x3_9
    val ex3_5_1 = "The output to ex3.5_1 is " + List.x3_5_1

    val ex3_12 = "The output to ex3.12 is " + List.x3_12
    val ex3_20 = "The output to ex3.20 is " + List.x3_20
    Ok(views.html.index(ex3_20))
  }


  //recursive utility function. Adds up all the numbers in the provided list.
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  //recursive utility function. Multiplies together all the numbers in the provided list.
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  //
  def apply[A](as: A*): List[A] = // Variadic function syntax, for the List datatype. Allows List to be created, as in 3.1
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))


  // exercise3.1: What will be the result of the following match expression?
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y  // This one fires!
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  //
  val x2 = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    //    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t) // this one fires, (using sum utility function)!
    case _ => 101
  }

  // Exercise 3.5: adds all elements of one list to another
  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  //Listing 3.2  Right folds and simple uses
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  //Listing 3.2 - Right Folds and simple example uses.
  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  //exercise 3.2 - Implement the function tail for removing the first element of a List .
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => List()
    case Cons(_, t) => t
  }
  val x3_2 = tail(List(1,2,3,4,5))


  //exercise 3.3 Using the same idea, implement the function setHead for replacing the first element of a List with a different value.
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => List()
    case Cons(_, t) => Cons(h, t)
  }
  val x3_3 = setHead(List(1,2,3,4,5), 45)


  //exercise 3.4 drops the first n list items: Tail recursive
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => List()
      case Cons (_, t) => drop(t, n-1)   //pass the tail back in again. Recursion is last thing, so it tail recursive.
    }
  }
  val x3_4 = drop(List(1,2,3,4,5), 3)


  // exercise 3.5
  // iterate thru list, drops matching items from the start of list, util the function returns false.
  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(h,t) if f(h) => dropWhile(t, f)
      case _ => l
    }
  }
  val x3_5 = dropWhile(List(1,2,3,4,5), (x: Int) => x < 3) // The type of x is explicitly defined


  // section 3.3.2
  // same as above, with currying, so we no longer need to specify the type of x.
  // dropWhile(xs) is returning a function, which we then call with the argument f
  // (in other words, dropWhile is curried)
  @annotation.tailrec
  def dropWhile2[A](as: List[A])(fn: A => Boolean): List[A] = {
    as match {
      case Cons(h,t) if fn(h) => dropWhile2(t) (fn)
      case _ => as
    }
  }
  val x3_5_1 = dropWhile2(List(1,2,3,4,5)) (x => x < 3) // The type of x is inferred now.




  //exercise 3.6. Return list with all but the last element
  def init[A](list: List[A]): List[A] =
    list match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => Nil
      case Cons(h,t) => Cons(h,init(t)) //not tail recursive, as Cons() depends upon init() to execute
    }
  val x3_6 = init(List(1,2,3,4,5))



  //exercise 3.8
  val x3_8 = foldRight(List(1,2,3), Nil:List[Int]) (Cons(_,_))



  //exercise 3.9
  // uses foldRight function to calculate the length of a List
  // The book doesn't really properly explain how foldRight works.
  // signature is foldRight(myList, initValue) (fn(listItem, prevResult) => ??)
  // performs the function with initValue and first list item
  // then performs function with result of the function with each of the remaining list items

  def length[A](l: List[A]): Int =
    foldRight(l, 0) ((_,acc) => acc + 1)

  val x3_9 = length(List(1,2,3,4,5,6))


  // copied here, to compare with foldLeft
  def _foldRight[A,B](list: List[A], runningResult: B)(fnc: (A, B) => B): B = list match {
    case Nil => runningResult
    case Cons(head, tail) => fnc(head, _foldRight(tail, runningResult)(fnc)) //function is on the outside, so recursion needs to be done first.
  }

  // exercise 3.10
  // foldRight is not tail-recursive and will result in a StackOverflowError for large lists
  // annotation just forces error, if not tail recursive.
  // In a tail recursive function, all calculations happen first and the recursive call is the last thing that happens.
  @annotation.tailrec
  def foldLeft[A,B](list: List[A], result: B)(fn: (B, A) => B): B = list match {
    case Nil => result
    case Cons(head,tail) => foldLeft(tail, fn(result,head)) (fn) //call the function, before invoking recursion
  }
/*
  The importance of this distinction doesn’t jump out at you, but it’s extremely important!
  Imagine a tail recursive function.  It runs.  It completes all its computation.
  As its very last action, it is ready to make its recursive call.
  What, at this point, is the use of the stack frame?  None at all.
  We don’t need our local variables anymore because we’re done with all computations.
  We don’t need to know which function we’re in because we’re just going to re-enter the very same function.
  Scala, in the case of tail recursion can eliminate the creation of a new stack frame and just re-use the current stack frame.
  The stack never gets any deeper, no matter how many times the recursive call is made.
  That’s the voodoo that makes tail recursion special in scala.
*/

  // exercise 3.11
  // Re-implement sum, prod and count using foldLeft
  def leftSum(ns: List[Int]) =
    foldLeft(ns, 0) ((x,y) => x + y)
  val x3_11_1 = leftSum(List(1,2,3,4,5,6))

  def leftProd(ns: List[Int]) =
    foldLeft(ns, 1)((x,y) => x * y)
  val x3_11_2 = leftProd(List(1,2,3,4,5,6))

  def leftCount(ns: List[Int]) =
    foldLeft(ns, 0)((_,y) => y + 1) - 1
  val x3_11_3 = leftCount(List(1,2,3,4,5,6))



//  def foldLeft[A,B](list: List[A], acc: B)(f: (B, A) => B): B

  // exercise 3.12
  def reverse[A](list: List[A]): List[A] =
    foldLeft(list, List[A]()) ((acc,h) => Cons(h,acc)) //define fn to take head & tail, to build list, from right-to-left
  val x3_12 = reverse(List(6,1,3,4,5,6))

  // exercise 3.14
  def fAppend[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((acc,h) => Cons(acc,h))
  val x3_14 = fAppend(List(1,2,3,4,5,6), List(7,8,9,10,11))

  // exercise 3.16
  // Write a function that transforms a list of integers by adding 1 to each element.
  // (Reminder: this should be a pure function that returns a new List !)
  def fAdd1(list: List[Int]): List[Int] =
    foldRight(list, Nil:List[Int]) ((h,t) => Cons(h+1,t))
  val x3_15 = fAdd1(List(1,2,3,4,5,6))

  // exercise 3.17
  // Write a function that turns each value in a List[Double] into a String .
  def dToString(a1: List[Double]): List[String] =
    foldRight(a1, Nil:List[String])((h,t) => Cons(h.toString,t))
  val x3_17 = dToString(List(1.0,2.0,3.0,4.0,5.0,6.0))


  //exercise 3.18
 //  Write a function map that generalizes modifying each element in a list while maintain-ing the structure of the list.
  def map1[A,B](list: List[A])(f: A => B): List[B] =
    foldRight(list, Nil:List[B])((h,t) => Cons(f(h),t))
  val x3_18 = map1(List(1,2,3,4,5,6)) (x => x+2)


  //exercise 3.19
  //Write a function filter that removes elements from a list unless they satisfy a given predicate.
  // Use it to remove all odd numbers from a List[Int].
  def filter1[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil:List[A]) ((h,t) => if (f(h)) Cons(h,t) else t)
  val x3_19 = filter1(List(1,2,3,4,5,6)) (x => x > 3)


  //utility function
  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil:List[Int])((h,t) => Cons(h+1,t))
  //utility function
  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil:List[String])((h,t) => Cons(h.toString,t))
  //utility function
  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((h,t) => Cons(f(h),t))

  //exercise 3.20
  // A function flatMap that works like map except that the function given will return
  // a list instead of a single result, and that list should be inserted into the final resulting
  //list
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))
  //utility function
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append)

  val x3_20 = flatMap(List(1,2,3,4,5,6)) (x => List(x+10))



  //exercise 3.21
  // Use flatMap to implement filter .
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  //exercise 3.22
  // A function that accepts two lists and constructs a new list by adding correspond-ing elements
  // For example, List(1,2,3) and List(4,5,6) become List(5,7,9) .
  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addPairwise(t1,t2))
  }


}