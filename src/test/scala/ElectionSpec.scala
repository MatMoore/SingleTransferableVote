import org.scalatest._
import stv._

class ElectionSpec extends FunSpec with Matchers {
  val apple = Candidate("Apple")
  val banana = Candidate("Banana")
  val candidates = Set(apple, banana)
  val newElection = Election(candidates)
  val appleVote = AllocatedVote(List(apple, banana), apple)
  val bananaVote = AllocatedVote(List(banana, apple), banana)
  def electedResult(candidate: Candidate, vote: AllocatedVote)(numVotes: Int) = {
    ElectedCandidate(candidate, List.fill(numVotes)(vote))
  }
  def hopefulResult(candidate: Candidate, vote: AllocatedVote)(numVotes: Int) = {
    HopefulCandidate(candidate, List.fill(numVotes)(vote))
  }
  val electedApple = electedResult(apple, appleVote)_
  val electedBanana = electedResult(banana, bananaVote)_
  val hopefulApple = hopefulResult(apple, appleVote)_
  val hopefulBanana = hopefulResult(banana, bananaVote)_
  val count = SomeCount(newElection)

  describe("Surplus reallocation") {
    // TODO: need more candidates to test this properly
    it("cannot change a result where no candidates are elected") {
      val hopeful = Set(hopefulApple(2), hopefulBanana(2))
      val initial = ElectionResult(List(), hopeful, Set())
      val result = count.reallocateAllSurplus(initial)
      result should equal (initial)
    }

    it("cannot change a result when the quota is met exactly") {
      val elected = List(electedApple(3))
      val hopeful = Set(hopefulBanana(1))
      val initial = ElectionResult(List(), hopeful, Set())
      val result = count.reallocateAllSurplus(initial)
      result should equal (initial)
    }

    it("can reallocate votes any time there is a surplus, even if all seats are filled") {
      val elected = List(electedApple(4))
      val initial = ElectionResult(elected, Set(), Set())
      val result = count.reallocateAllSurplus(initial)
      result shouldNot equal (initial)

      // One of apples votes is reallocated to banana
      result.hopefuls should equal (Set(HopefulCandidate(banana, List(AllocatedVote(List(apple, banana), banana)))))
    }

  }
  describe("Eliminating a candidate") {
    describe("when all candidates are elected or eliminated") {
      val eliminated: Set[EliminatedCandidate] = Set(EliminatedCandidate(banana))
      val elected: List[ElectedCandidate] = List(electedApple(1))
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
        val result = SomeCount(newElection).eliminate(ElectionResult(elected, Set(hopefulApple(1)), eliminated))
        result.eliminated should equal (Set(EliminatedCandidate(apple)))
      }

      describe("when apple has two votes and banana has one vote") {
        val result = SomeCount(newElection).eliminate(ElectionResult(elected, Set(hopefulApple(2), hopefulBanana(1)), eliminated))

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
