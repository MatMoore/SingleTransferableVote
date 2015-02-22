package stv

case class Candidate(name: String)

/**
 * An election comprises a set of candidates and the votes for them.
 * @param candidates All candidates that can be voted for.
 * @param votes A list of votes, where each vote is a list ranking candidates in order of preference.
 * @param seats The number of seats to be filled by winning candidates.
 */
case class Election(candidates: Set[Candidate], votes: List[List[Candidate]] = List(), seats: Int = 1) {
  def vote(ranking: List[Candidate]) = {
    require {ranking.forall(candidates.contains(_))}
    Election(candidates, ranking :: votes.toList)
  }
}

/**
 * The state of a ballot at any given time.
 * @param vote The original vote.
 * @param selected The candidate the vote is currently allocated to
 *                 (the vote may be transferred to less preferred candidates
 *                 throughout the counting process).
 */
case class AllocatedVote(vote: List[Candidate], selected: Candidate)

/**
 * The method of dealing with surplus votes for a winning candidate.
 * Surplus votes for an elected candidate should be transferred to another
 * preferred candidate to avoid wasted votes.
 */
trait SurplusVoteAllocator {
  /**
   * Reassign surplus votes from the donors to the receivers.
   * It is up to the implementation whether to allow transfers to already elected candidates.
   * @param quota number of votes required to become elected
   * @param donors list of elected candidates
   * @param receivers list of unelected candidates
   * @return
   */
  def reallocateFromDonors(
    quota: Int,
    donors: Iterable[ElectedCandidate],
    receivers: Iterable[CandidateResult]
  ): Tuple2[List[CandidateResult], Boolean]

  def reallocate(quota: Int, initial: Iterable[CandidateResult]): Tuple2[List[CandidateResult], Boolean] = {
    val elected = initial.collect { case ElectedCandidate(c, v) => ElectedCandidate(c, v)}
    val hopeful = initial.collect { case HopefulCandidate(c, v) => HopefulCandidate(c, v)}
    reallocateFromDonors(quota, elected, hopeful)
  }
}

/**
 * The result of a round of counting for a single candidate.
 */
sealed trait CandidateResult

/**
 * A candidate that has met the quota to be elected.
 * @param candidate The elected candidate.
 * @param votes The number of votes currently allocated to the candidate.
 */
final case class ElectedCandidate(candidate: Candidate, votes: List[AllocatedVote]) extends CandidateResult

/**
 * A candidate that has not yet met the quota to be elected.
 * @param candidate The unelected candidate
 * @param votes The number of votes currently allocated to the candidate.
 */
final case class HopefulCandidate(candidate: Candidate, votes: List[AllocatedVote] = List()) extends CandidateResult

/**
 * A candidate that has been eliminated from the election.
 * @param candidate The eliminated candidate.
 */
final case class EliminatedCandidate(candidate: Candidate) extends CandidateResult

/**
 * Determines the result of an election.
 */
trait Count {
  val election: Election
  val numVotes: Int = election.votes.length
  val numSeats: Int = election.seats

  /**
   * Counting STV requires some method of defining a vote quota for candidates
   * to be elected. Two standard quota implementations are defined in this trait:
   * The Droop Quota and the Hare Quota.
   * @return The minimum number of votes a candidate must have to be elected.
   */
  def quota: Int

  // TODO: these may be off by one due to integer division

  /**
   * Fixed Droop Quota for the election.
   */
  lazy val droopQuota: Int = numVotes / (numSeats + 1) + 1

  /**
   * Fixed Hare Quota for the election
   */
  lazy val hareQuota: Int = numVotes / numSeats

  /**
   * Method of reallocating surplus votes from elected candidates.
   */
  def surplusAllocator: SurplusVoteAllocator

  /**
   * Count the election.
   * @return A list containing the results per candidate at the end of each
   *         round.
   */
  def roundCounts: List[List[CandidateResult]] = {
    def eliminate(candidates: List[CandidateResult]): List[CandidateResult]

    def isComplete(currentRound: List[CandidateResult]): Boolean = true

    val firstChoiceVotes: Map[Candidate, List[AllocatedVote]] = {
      val allocated = election.votes.map(vote => AllocatedVote(vote, vote.head))
      allocated.groupBy(_ match {
        case AllocatedVote(vote, selected) => selected
      })
    }

    val initialResults: List[CandidateResult] = (for {
      (candidate: Candidate, votes: List[AllocatedVote]) <- firstChoiceVotes
    } yield {
      if (votes.length > quota) ElectedCandidate(candidate, votes) else HopefulCandidate(candidate, votes)
    }).toList

    def nextRound(currentRound: List[CandidateResult]): List[CandidateResult] = {
      val (reallocatedSurplusResults, newlyElected) = surplusAllocator.reallocate(quota, initialResults)

      if(newlyElected) {
        reallocatedSurplusResults
      } else {
        eliminate(reallocatedSurplusResults)
      }
    }

    var rounds: List[List[CandidateResult]] = List(initialResults)
    var currentRound = initialResults
    while(!isComplete(currentRound)) {
      currentRound = nextRound(currentRound).toList
      rounds = rounds :+ currentRound
    }

    rounds
  }

  /**
   * Count the election.
   * @return The results per candidate after the vote is counted.
   */
  def finalCount: List[CandidateResult] = roundCounts.last

  /**
   * Determine who wins each seat.
   * @return The candidates who fill each seat after the vote is counted.
   */
  def finalSeats: List[Option[ElectedCandidate]] = {
    val filledSeats: List[Some[ElectedCandidate]] = finalCount.take(numSeats).collect {
        case ElectedCandidate(candidate, votes) => Some(ElectedCandidate(candidate, votes))
    }
    filledSeats ++ List.fill(numSeats - filledSeats.length) {None}
  }
}

object Test extends App {
  val foo = Candidate("Foo")
  val bar = Candidate("Bar")
  var election = new Election(Set(foo, bar))
  election = election.vote(List(foo, bar))
  println(election.votes)
}