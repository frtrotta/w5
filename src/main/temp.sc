def squareList(xs: List[Int]): List[Int] =
  xs match {
    case Nil => Nil
    case y :: ys => (y * y) :: squareList(ys)
  }

def sqrLst(xs : List[Int]): List[Int] =
  xs map (x => x*x)

squareList(List(1,2,3))
sqrLst(List(1,2,3))

def pack_c[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 => xs span (y => y == x) match {
    case (Nil, a) => pack_c(a) // This case is not relevant
    case (a, Nil) => List(a) // This case is not relevant
    case (a, b) => a :: pack_c(b)
  }
}

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case h :: tail =>
    val (first, rest) = xs span (y => y == h)
    first :: pack(rest)
}

def encode_c[T](xs: List[T]): List[(T, Int)] = {
  def encodeHelper(ys: List[List[T]]): List[(T, Int)] =
    ys match {
      case Nil => Nil
      case h :: tail => (h(0), h.length) :: encodeHelper(tail)
    }

  encodeHelper(pack(xs))
}

def encode[T](xs: List[T]) = pack(xs) map (y => (y(0), y.length))

pack(List(1,1,2,0,0,0,4))

encode(List(1,1,2,0,0,0,4))

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())( (x, y) => f(x) :: y )

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)( ??? )


/*def flatten(xs: List[Any]): List[Any] =
  xs match {
    case List() => xs
    case y :: ys => y match {
      case z :: zs => flatten(y) ++ flatten(ys)
      case z => y :: flatten(ys)
    }
  }

flatten(List(List(1, 1), 2, List(3, List(5, 8))))*/