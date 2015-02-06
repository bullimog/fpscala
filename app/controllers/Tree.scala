package controllers

import play.api.mvc._

/**
 * Created by graeme on 25/01/15.
 */

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree extends Controller {

  def index = Action {
    val ex3_25 = "The output from ex3_25 is " + Tree.ex3_25
    val ex3_26 = "The output from ex3_26 is " + Tree.ex3_26
    val ex3_27 = "The output from ex3_27 is " + Tree.ex3_27
    val ex3_28 = "The output from ex3_28 is " + Tree.ex3_28
    Ok(views.html.index(ex3_28))
  }

  //exercise 3.25
  // Write a function size that counts the number of nodes (leaves and branches) in a tree.
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }

  val tree = Branch(Leaf(1), Branch(Leaf(2),Branch(Leaf(3),Leaf(4))))
  val ex3_25 = size(tree)

  /*
  exercise 3.26
  We're using the method `max` that exists on all `Int` values rather than an explicit `if` expression.
  Note how similar the implementation is to `size`. We'll abstract out the common pattern in a later exercise.
  */
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(l,r) => maximum(l) max maximum(r)
  }
  val ex3_26 = maximum(tree)

  /*
  Exercise 3.27
  Write a function depth that returns the maximum path length from the root of a tree to any leaf.
  Again, note how similar the implementation is to `size` and `maximum`.
  */
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l,r) => 1 + (depth(l) max depth(r))
  }
  val ex3_27 = depth(tree)


  /*
  Excercise 3.28
  Write a function map , analogous to the method of the same name on List , that modifies
  each element in a tree with a given function.
  */
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }
  val ex3_28 = map(tree) (x=>x+10)

  /*
  Excercise 3.29
  Generalize size , maximum , depth , and map , writing a new function fold that abstracts over their similarities.
  Like `foldRight` for lists, `fold` receives a "handler" for each of the data constructors of the type, and recursively accumulates some value using these handlers. As with `foldRight`, `fold(t)(Leaf(_))(Branch(_,_)) == t`, and we can use this function to implement just about any recursive function that would otherwise be defined by pattern matching.
  */
  def fold[A,B](tree: Tree[A])(leafFn: A => B)(branchFn: (B,B) => B): B = tree match {
    case Leaf(leafVal) => leafFn(leafVal)
    case Branch(l,r) => branchFn(fold(l)(leafFn)(branchFn), fold(r)(leafFn)(branchFn))
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(a => 1)(1 + _ + _)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(a => a)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(a => 0)((d1,d2) => 1 + (d1 max d2))

  /*
  Note the type annotation required on the expression `Leaf(f(a))`. Without this annotation, we get an error like this:

  type mismatch;
    found   : fpinscala.datastructures.Branch[B]
    required: fpinscala.datastructures.Leaf[B]
       fold(t)(a => Leaf(f(a)))(Branch(_,_))
                                      ^

  This error is an unfortunate consequence of Scala using subtyping to encode algebraic data types. Without the annotation, the result type of the fold gets inferred as `Leaf[B]` and it is then expected that the second argument to `fold` will return `Leaf[B]`, which it doesn't (it returns `Branch[B]`). Really, we'd prefer Scala to infer `Tree[B]` as the result type in both cases. When working with algebraic data types in Scala, it's somewhat common to define helper functions that simply call the corresponding data constructors but give the less specific result type:

    def leaf[A](a: A): Tree[A] = Leaf(a)
    def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l, r)
  */
  def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))

}
