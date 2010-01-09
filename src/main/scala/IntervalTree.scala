class IntervalTree {
  case class Interval(start: Int, end: Int) extends Ordered[Interval] {
    override def equals(other: Any) = other match { // This equality rule is unhashable
      case that: Interval => this <= that && that <= this
      case _ => false
    }
    def compare(that: Interval) = (start compare that.start, end compare that.end) match {
      case (-1, -1) => -1
      case (1, 1) => 1
      case _ => 0
    }
    def contains(p: Int) = start <= p && p <= end
    def center = start + (end - start) / 2
    lazy val centralized = Interval(center, center) // Must be either lazy or def to avoid infinite recursion
  }
  object Interval {
    implicit def fromRange(r: Range) = Interval(r.first, r.last)
    implicit def toRange(i: Interval) = i.start to i.end
  }
  
  case class Node(byStart: List[Interval], byEnd: List[Interval]) {
    def this(i: Interval) = this(List(i), List(i))
    def this(point: Int) = this(Interval(point, point))
    def insert(i: Interval): Node = {
      if (byStart contains i) return this
      val (byStartBefore, byStartAfter) = byStart span (_.start < i.start)
      val (byEndBefore, byEndAfter) = byEnd span (_.end < i.end)
      Node(byStartBefore ::: i :: byStartAfter, byEndBefore ::: i :: byEndAfter)
    }
    def delete(i: Interval): Node = {
      if (!byStart.contains(i)) return this
      Node(byStart filter (_ != i), byEnd filter (_ != i))
    }
  }
  object Node {
    def apply(i: Interval) = new Node(i)
    def apply(point: Int) = new Node(point)
  }
  
  object RedBlackInterval extends scala.collection.immutable.RedBlack[Interval] {
    def isSmaller(x: Interval, y: Interval) = x < y
  }
  import RedBlackInterval._
  
  var root: Tree[Node] = Empty
  
  def insert(i: Interval) = {
      root lookup i match {
      case Empty => root = root update (i.centralized, Node(i))
      case ne: NonEmpty[_] => root = root update (ne.key, ne.value insert i)
    }
    this
  }
  
  def delete(i: Interval) = {
    root lookup i match {
      case Empty => ()
      case ne: NonEmpty[_] =>
        val newNode = ne.value delete i
        if (newNode.byStart.isEmpty)
          root = root delete i
        else
          root = root update (ne.key, newNode)
    }
    this
  }
  
  def lookup(i: Interval) = root lookup i match {
    case Empty => (Nil, Nil)
    case ne: NonEmpty[_] => (ne.value.byStart, ne.value.byEnd)
  }
  def lookup(point: Int): (List[Interval], List[Interval]) = lookup(Interval(point, point))
  
  def range(from: Option[Interval], to: Option[Interval]) = 
    root.range(from, to).iterator.foldLeft((Nil, Nil): (List[Interval], List[Interval])) {
      case ((bS, bE), (_, n)) => (bS ::: n.byStart, bE ::: n.byEnd)
    }
    
  def iterator = root.elements map (n => (n._2.byStart, n._2.byEnd))
  def iteratorByStart = root.elements flatMap (_._2.byStart.elements)
  def iteratorByEnd = root.elements flatMap (_._2.byEnd.elements)
}

