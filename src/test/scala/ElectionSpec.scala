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
    describe("when all candidates are elected or eliminated") {
      val eliminated: Set[EliminatedCandidate] = Set(EliminatedCandidate(banana))
      val elected: List[ElectedCandidate] = List(ElectedCandidate(apple, List(appleVote)))
      it("should no longer be possible to eliminate") {
        intercept[IllegalArgumentException] {
          SomeCount(newElection).eliminate(ElectionResult(elected, Set(), eliminated))
        }
      }
    }
    describe("when nobody has been eliminated or elected yet") {
      val eliminated: Set[EliminatedCandidate] = Set()
      val elected: List[ElectedCandidate] = List()

      it("an election must have hopeful candidates") {
        intercept[IllegalArgumentException] {
          ElectionResult(elected, Set(), eliminated)
        }
      }

      it("can eliminate the only hopeful") {
        val hopefulApple = HopefulCandidate(apple, List(appleVote))
        val result = SomeCount(newElection).eliminate(ElectionResult(elected, Set(hopefulApple), eliminated))
        result.eliminated should equal (Set(EliminatedCandidate(apple)))
      }

      describe("when apple has two votes and banana has one vote") {
        val hopefulApple = HopefulCandidate(apple, List(appleVote, appleVote))
        val hopefulBanana = HopefulCandidate(banana, List(bananaVote))
        val result = SomeCount(newElection).eliminate(ElectionResult(elected, Set(hopefulApple, hopefulBanana), eliminated))

        it("should remove banana") {
          result.eliminated should equal (Set(EliminatedCandidate(banana)))
        }

        it("should reallocate the banana vote to apple, causing it to become elected") {
          result.elected should equal (List(
            ElectedCandidate(
              apple,
              List(appleVote, appleVote, AllocatedVote(List(banana, apple), apple))
            )
          ))
        }

        it("should only contain two candidates") {
          result.hopefuls shouldBe empty
        }
      }
    }
  }
}
