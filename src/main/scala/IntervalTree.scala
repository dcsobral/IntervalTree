class IntervalTree {
  /**
   * Add a few methods to Range useful in implementing the Black&Red Tree
   */
  case class Interval(start: Int, end: Int) extends Ordered[Interval] {
    def compare(that: Interval) = (start compare that.start, end compare that.end) match {
      case (-1, -1) => -1
      case (1, 1) => 1
      case _ => 0
    }
    def contains(point: Int) = start <= point && point <= end
    def intersects(that: Interval) = (that contains start) || (that contains end)
    val center = start + (end - start) / 2
    def centralized = Interval(center, center) // Must be either lazy or def to avoid infinite recursion
  }
  object Interval {
    implicit def fromRange(r: Range) = Interval(r.first, r.last)
    implicit def toRange(i: Interval) = i.start to i.end
  }

  import Interval._ // Add "Interval" methods to Range
  
  case class Node(byStart: List[Range], byEnd: List[Range]) {
    def this(r: Range) = this(List(r), List(r))
    def this(point: Int) = this(point to point)
    def insert(r: Range): Node = {
      if (byStart contains r) return this
      val (byStartBefore, byStartAfter) = byStart span (_.head < r.head)
      val (byEndBefore, byEndAfter) = byEnd span (_.last < r.last)
      Node(byStartBefore ::: r :: byStartAfter, byEndBefore ::: r :: byEndAfter)
    }
    def delete(r: Range): Node = {
      if (!byStart.contains(r)) return this
      Node(byStart filter (_ != r), byEnd filter (_ != r))
    }
  }
  object Node {
    def apply(r: Range) = new Node(r)
    def apply(point: Int) = new Node(point)
  }
  
  object RedBlackInterval extends scala.collection.immutable.RedBlack[Range] {
    def isSmaller(x: Range, y: Range) = x < y
  }
  import RedBlackInterval._
  
  var root: Tree[Node] = Empty
  
  def insert(r: Range) = {
      root lookup r match {
      case Empty => root = root update (r.centralized, Node(r))
      case ne: NonEmpty[_] => root = root update (ne.key, ne.value insert r)
    }
    this
  }
  
  def delete(r: Range) = {
    root lookup r match {
      case Empty => ()
      case ne: NonEmpty[_] =>
        val newNode = ne.value delete r
        if (newNode.byStart.isEmpty)
          root = root delete r
        else
          root = root update (ne.key, newNode)
    }
    this
  }
  
  def lookup(r: Range) = {
    def search(t: Tree[Node]): (List[Range], List[Range]) = t match {
      case Empty => (Nil, Nil)
      case ne: NonEmpty[_] =>
        val (byStart, byEnd) = if (r < ne.key) search(ne.left) else search(ne.right)
        ((ne.value.byStart filter (r intersects _)) ++ byStart,
         (ne.value.byEnd filter (r intersects _)) ++ byEnd)
    }
    search(root)
  }
  def lookup(point: Int): (List[Range], List[Range]) = lookup(point to point)
  
  def range(from: Option[Range], to: Option[Range]) =
    root.range(from, to).iterator.foldLeft((Nil, Nil): (List[Range], List[Range])) {
      case ((bS, bE), (_, n)) => (bS ::: n.byStart, bE ::: n.byEnd)
    }
    
  def iterator = root.elements map (n => (n._2.byStart, n._2.byEnd))
  def iteratorByStart = root.elements flatMap (_._2.byStart.elements)
  def iteratorByEnd = root.elements flatMap (_._2.byEnd.elements)

  def rangeToString(r: Range) = "[%d%s]" format (r.first, if (r.last == r.first) "" else ","+r.last)

  def treeToString(t: Tree[Node], level: Int): String = t match {
    case Empty => "E\n"
    case BlackTree(key, value, left, right) =>
      "Black Center: %s\tRanges: %s\n%s|\n%s+L: %s%s|\n%s+R: %s" format
        (key, value.byStart map rangeToString mkString ",",
         "|"*level,
         "|"*level, treeToString(left, level + 1),
         "|"*level,
         "|"*level, treeToString(right, level + 1))
    case RedTree(key, value, left, right) =>
      "Red Center: %s\tRanges: %s\n%s|\n%s+L: %s%s|\n%s+R: %s" format
        (key, value.byStart map rangeToString mkString ",",
         "|"*level,
         "|"*level, treeToString(left, level + 1),
         "|"*level,
         "|"*level, treeToString(right, level + 1))
    case other => error("Unexpected "+other)
  }

  override def toString = "\n"+treeToString(root, 0)
}

