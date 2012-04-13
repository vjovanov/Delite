package functionalFun

trait Lift[F[_]] {

  def lift0[A]: A => F[A] //unit

  def lift1[A, B]: (A => B) => (F[A] => F[B]) //fmap

  def lift2[A, B, C]: (A => B => C) => (F[A] => F[B] => F[C])
  
  def lift3[A, B, C, D]: (A => B => C => D) => (F[A] => F[B] => F[C] => F[D])

  //relationship between lift<N> and lift<N-1>
  def ap[A, B]: F[A => B] => (F[A] => F[B])
}

trait LiftImpl[F[_]] extends Lift[F] {
  //each lift function uses previous lift function and ap
  def lift1[A, B] = ap compose lift0

  def lift2[A, B, C] = f => ap compose lift1(f)
  
  def lift3[A, B, C, D] = f => g => ap compose lift2(f)(g)
}

class ListLift extends LiftImpl[List] {
  def lift0[A] = a => List(a)

  def ap[A, B]: List[A => B] => (List[A] => List[B]) =
    lab => (la => lab.flatMap(f => la.map(a => f(a))))

}

class OptionLift extends LiftImpl[Option] {
  def lift0[A] = a => Option(a)

  def ap[A, B]: Option[A => B] => (Option[A] => Option[B]) =
    oab => (oa => oab.flatMap(f => oa.map(a => f(a))))
}

class Function1Lift[R] extends LiftImpl[({type F[A] = R => A})#F] {
  def lift0[A] = a => r => a
  
  def ap[A,B]: (R => A => B) => ((R => A) => (R => B)) =
    f => g => (r => f(r)(g(r)))
}

class EitherLift[R] extends LiftImpl[({type E[A] = Either[R,A]})#E] {
  def lift0[A] = a => Right(a)
  
  def ap[A,B]: Either[R, A => B] => (Either[R,A] => Either[R,B]) = 
    eab => (ea => eab.fold(Left(_), f => ea.fold(Left(_), a => Right(f(a)))))
}
