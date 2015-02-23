import org.scalatest._
import stv._

class ElectionSpec extends FunSpec with Matchers {
  val apple = Candidate("Apple")
  val banana = Candidate("Banana")
  val candidates = Set(apple, banana)
  val newElection = Election(candidates)
  val appleVote = AllocatedVote(List(apple, banana), apple)
  val bananaVote = AllocatedVote(List(banana, apple), banana)

  describe("Eliminating a candidate") {
    describe("when nobody has been eliminated or elected yet") {
      val eliminated: Set[EliminatedCandidate] = Set()
      val elected: List[ElectedCandidate] = List()

      it("should require at least one hopeful candidate") {
        intercept[IllegalArgumentException] {
          SomeCount(newElection).eliminate(elected, List(), eliminated)
        }
      }

      it("can eliminate the only hopeful") {
        val hopefulApple = HopefulCandidate(apple, List(appleVote))
        val result = SomeCount(newElection).eliminate(elected, List(hopefulApple), eliminated)
        result should equal (List(EliminatedCandidate(apple)))
      }

      describe("when apple has two votes and banana has one vote") {
        val hopefulApple = HopefulCandidate(apple, List(appleVote, appleVote))
        val hopefulBanana = HopefulCandidate(banana, List(bananaVote))
        val result = SomeCount(newElection).eliminate(elected, List(hopefulApple, hopefulBanana), eliminated).toSet

        it("should remove banana") {
          result should contain (EliminatedCandidate(banana))
        }

        it("should reallocate the banana vote to apple, causing it to become elected") {
          result should contain (ElectedCandidate(apple, List(appleVote, appleVote, AllocatedVote(List(banana, apple), apple))))
        }

        it("should only contain two candidates") {
          result should have size 2
        }
      }
    }
  }
}
