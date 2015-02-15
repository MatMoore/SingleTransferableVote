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
  def reallocate(quota: Int, toReallocate: List[AllocatedVote]): List[AllocatedVote]
}

/**
 * The result of a round of counting for a single candidate.
 */
trait CandidateResult

/**
 * A candidate that has met the quota to be elected.
 * @param candidate The elected candidate.
 * @param votes The number of votes currently allocated to the candidate.
 */
case class ElectedCandidate(candidate: Candidate, votes: Int) extends CandidateResult

/**
 * A candidate that has not yet met the quota to be elected.
 * @param candidate The unelected candidate
 * @param votes The number of votes currently allocated to the candidate.
 */
case class HopefulCandidate(candidate: Candidate, votes: Int = 0) extends CandidateResult

/**
 * A candidate that has been eliminated from the election.
 * @param candidate The eliminated candidate.
 */
case class EliminatedCandidate(candidate: Candidate) extends CandidateResult

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
  def roundCounts: List[List[CandidateResult]]

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