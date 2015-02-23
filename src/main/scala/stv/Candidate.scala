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
case class AllocatedVote(vote: List[Candidate], selected: Candidate) {
  /**
   * Transfer the vote to the next best choice
   * @param eliminated All candidates that have been eliminated from the election
   * @return New allocation for this vote
   */
  def transfer(eliminated: Set[Candidate]): Option[AllocatedVote] = {
    vote.filter(other => !eliminated.contains(other)) match {
      case head :: tail => Some(AllocatedVote(vote, head))
      case nil => None
    }
  }
}

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
  def meetsQuota(numVotes: Int): Boolean = numVotes > quota
  def meetsQuota(votes: List[AllocatedVote]): Boolean = meetsQuota(votes.length)

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
   * Eliminate a single candidate.
   * @param elected All previously elected candidates
   * @param hopefuls All candidates that have not been elected or eliminated yet.
   * @param eliminated All previously eliminated candidates
   * @return A new list of candidate results.
   */
  def eliminate(elected: List[ElectedCandidate], hopefuls: List[HopefulCandidate], eliminated: Set[EliminatedCandidate]): List[CandidateResult] = {
    require(hopefuls.length > 0)

    val eliminatedSet = eliminated.map(_.candidate)
    val loser = hopefuls.minBy(candidate => candidate.votes.length)
    val newEliminatedSet = eliminatedSet + loser.candidate
    val loserVotes = loser.votes.map(_.transfer(newEliminatedSet)).collect {
      case Some(allocatedVote) => allocatedVote
    }
    val loserVotesByCandidate = loserVotes.groupBy(_.selected).withDefaultValue(List())

    val stillElected = for (
      ElectedCandidate(candidate, votes) <- elected
    ) yield ElectedCandidate(candidate, votes ++ loserVotesByCandidate(candidate))

    val formerHopefuls = for (
      HopefulCandidate(candidate, votes) <- hopefuls
      if HopefulCandidate(candidate, votes) != loser
    ) yield {
      val newVotes = votes ++ loserVotesByCandidate(candidate)
      if (meetsQuota(newVotes)) {
        ElectedCandidate(candidate, newVotes)
      } else {
        HopefulCandidate(candidate, newVotes)
      }
    }

    /*
    The way intermediate CandidateResults are stored is currently a bit haphazard.
    For now just lump everything into a list
     */
    EliminatedCandidate(loser.candidate) :: (stillElected.toList ++ formerHopefuls.toList)
  }

  /**
   * Count the election.
   * @return A list containing the results per candidate at the end of each
   *         round.
   */
  def roundCounts: List[List[CandidateResult]] = {
    def isComplete(currentRound: List[CandidateResult]): Boolean = true

    /*
    Start off with every ballot allocated to the first choice candidate
     */
    val firstChoiceVotes: Map[Candidate, List[AllocatedVote]] = {
      val allocated = election.votes.map(vote => AllocatedVote(vote, vote.head))
      allocated.groupBy(_ match {
        case AllocatedVote(vote, selected) => selected
      })
    }

    /*
    Elect any candidates that meet the criteria
     */
    val initialResults: List[CandidateResult] = (for {
      (candidate: Candidate, votes: List[AllocatedVote]) <- firstChoiceVotes
    } yield {
      if (votes.length > quota) ElectedCandidate(candidate, votes) else HopefulCandidate(candidate, votes)
    }).toList

    /**
     *
      In each round of counting, we either:
      1) Transfer surplus votes from newly elected candidates
      2) Eliminate candidate and transfer their votes
      We only eliminate when no new candidates can be elected through step 1.
     * @param currentRound The results as they stand so far.
     * @return The results after running a new round of counting.
     */
    def nextRound(currentRound: List[CandidateResult]): List[CandidateResult] = {
      val (reallocatedSurplusResults, newlyElected) = surplusAllocator.reallocate(quota, initialResults)

      if(newlyElected) {
        reallocatedSurplusResults
      } else {
        /*
        FIXME: this is not very nice
         */
        val elected = (reallocatedSurplusResults.collect {
          case ElectedCandidate(candidate, votes) => ElectedCandidate(candidate, votes)
        }).toList
        val hopeful = (reallocatedSurplusResults collect {
          case HopefulCandidate(candidate, votes) => HopefulCandidate(candidate, votes)
        }).toList
        val eliminated = (reallocatedSurplusResults collect {
          case EliminatedCandidate(candidate) => EliminatedCandidate(candidate)
        }).toSet

        eliminate(elected, hopeful, eliminated)
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

case class SomeCount(election: Election) extends Count {
  val quota = droopQuota
  val surplusAllocator = null // TODO
}

object Test extends App {
  val foo = Candidate("Foo")
  val bar = Candidate("Bar")
  var election = new Election(Set(foo, bar))
  election = election.vote(List(foo, bar))
  println(election.votes)
}