package datastructure

import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // 3.25
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(v)      => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  // 3.26
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(v)      => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  //3.27  
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_)      => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  // 3.28
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    case Leaf(v)      => Leaf(f(v))
  }

  // 3.29 fold
  def fold[A, B](tree: Tree[A])(l: A => B)(b: (B, B) => B): B = tree match {
    case Branch(lb, rb) => b(fold(lb)(l)(b), fold(rb)(l)(b))
    case Leaf(v)        => l(v)
  }

  def sizeViaFold[A](tree: Tree[A]): Int = fold(tree)(a => 1)(_ + _ + 1)

  def maximumViaFold(tree: Tree[Int]): Int = fold(tree)(a => a)(_ max _)

  def depthViaFold[A](tree: Tree[A]): Int = fold(tree)(a => 0)((c, d) => 1 + (c max d))

  def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

}