import org.scalacheck._
import Prop._
import Arbitrary.arbitrary

object IntervalTreeCheck extends Properties("IntervalTree") {
  val smallInteger = Gen.choose(0,500) // Avoid stack overflows causes by toString on very big ranges
  val myRangeGen = for {
    start <- smallInteger
    end <- smallInteger
  } yield if (start < end) start to end else end to start
  implicit def arbRange: Arbitrary[Range] = Arbitrary(myRangeGen)
  
  val myIntervalTreeGen = for {
    l <- Gen.listOf(myRangeGen)
  } yield l.foldLeft(new IntervalTree){ (t, r) => t insert r; t }
  implicit def arbIntervalTree: Arbitrary[IntervalTree] = Arbitrary(myIntervalTreeGen)
  
  property("All intervals containing a point are returned") =
    forAll { (t: IntervalTree, n: Int) => n < 1000 ==>
      (t.iteratorByStart.toList.filter(_ contains n) == t.lookup(n)._1) :|
        "Expected: "+(t.iteratorByStart.toList.filter(_ contains n).map(t.rangeToString)+
        "\nGot: "+t.lookup(n)._1.map(t.rangeToString))
    }
}
