package functionalFun.exercises

import scala.Either.{RightProjection, LeftProjection}


trait PartialType[T[_, _], A] {
  type Apply[B] = T[A, B]
  type Flip[B] = T[B, A]
}
 
trait Functor[F[_]] {
  def map[A, B](fa: F[A], f: A => B): F[B]
}
 
object Functor {
  
  def ListFunctor: Functor[List] = new Functor[List] {
    def map[A, B](fa: List[A], f: A => B): List[B] = fa.map(f)
  } 
 
  def OptionFunctor: Functor[Option] = new Functor[Option] {
    def map[A, B](fa: Option[A], f: (A) => B): Option[B] = fa.map(f)
  }
 
  def Function1Functor[X]: Functor[PartialType[Function1, X]#Apply] = new Functor[PartialType[Function1, X]#Apply] {
    def map[A, B](fa: X => A, f: A => B): X => B = f compose fa
  }
 
  def EitherLeftFunctor[X]: Functor[PartialType[Either.LeftProjection, X]#Flip] = new Functor[PartialType[Either.LeftProjection, X]#Flip] {
    def map[A, B](fa: LeftProjection[A,X], f: A => B): Either.LeftProjection[B,X] = LeftProjection(fa.map(f))
  }
 
  def EitherRightFunctor[X]: Functor[PartialType[Either.RightProjection, X]#Apply] = new Functor[PartialType[RightProjection, X]#Apply] {
    def map[A, B](fa: RightProjection[X,A], f: A => B): Either.RightProjection[X,B] = RightProjection(fa.map(f))
  }

}
 
trait Monad[M[_]] extends Functor[M] {
  def flatMap[A, B](ma: M[A], f: A => M[B]): M[B]
 
  def unit[A](a: A): M[A]
 
  // (use flatMap and/or unit)
  def map[A, B](ma: M[A], f: A => B): M[B] = 
    flatMap(ma, (a:A) => unit(f(a)))
}
 
object Monad {

  def ListMonad: Monad[List] = new Monad[List] {
    def unit[A](a: A): List[A] = List(a)
    def flatMap[A, B](ma: List[A], f: (A) => List[B]): List[B] = ma.flatMap(f)
  }
 
  def OptionMonad: Monad[Option] = new Monad[Option] {
    def unit[A](a: A): Option[A] = Option(a)
    def flatMap[A, B](ma: Option[A], f: (A) => Option[B]): Option[B] = ma.flatMap(f)
  }
 
  def Function1Monad[X]: Monad[PartialType[Function1, X]#Apply] = new Monad[PartialType[Function1, X]#Apply] {
    def unit[A](a: A): X => A = x => a
    def flatMap[A, B](ma: X => A, f: A => X => B): X => B = x => f(ma(x))(x)
  }
 
  def EitherLeftMonad[X]: Monad[PartialType[Either.LeftProjection, X]#Flip] =
    error("todo")
 
  def EitherRightMonad[X]: Monad[PartialType[Either.RightProjection, X]#Apply] =
    error("todo")
 
  def flatten[M[_], A](ma: M[M[A]], m: Monad[M]): M[A] =
    m.flatMap(ma, (a:M[A]) => a)
 
  def apply[M[_], A, B](ma: M[A], mf: M[A => B], m: Monad[M]): M[B] =
    m.flatMap(mf, (ff:A=>B) => m.map(ma, ff))
 
  def sequence[M[_], A, B](as: List[A], f: A => M[B], m: Monad[M]): M[List[B]] = 
    error("todo")
  
  def filter[M[_], A](as: List[A], f: A => M[Boolean], m: Monad[M]): M[List[A]] = as match {
    case Nil => m.unit(Nil)
    case h::t => m.flatMap(f(h), (z: Boolean) =>
      m.map(filter(t, f, m), (k:List[A]) => if (z) h::k else k))
  }
 
  def replicate[M[_], A](n: Int, a: M[A], m: Monad[M]): M[List[A]] =
    error("todo")

  def lift2[M[_], A, B, C](f: (A, B) => C, a: M[A], b: M[B], m: Monad[M]): M[C] =
    m.flatMap(a, (aa:A) => m.map(b, (bb:B) => f(aa,bb)))
  
}
 
object AdvancedFun {
  case class State[S, A](f: S => (S, A))

  def StateFunctor[S]: Functor[PartialType[State, S]#Apply] = new Functor[PartialType[State, S]#Apply] {
    def map[A, B](fa: State[S,A], f: A => B): State[S,B] = State { s => 
      val (ns,v) = fa.f(s)
      (ns, f(v))
    }
  }

  def StateMonad[S]: Monad[PartialType[State, S]#Apply] = new Monad[PartialType[State, S]#Apply] {
    def unit[A](a: A): State[S,A] = 
      State(s => (s,a))
   
    def flatMap[A, B](ma: State[S,A], f: A => State[S, B]): State[S,B] = State { s =>
      val (ns,v) = ma.f(s)
      val nState = f(v)
      nState.f(ns)
    }
  }
  
}
