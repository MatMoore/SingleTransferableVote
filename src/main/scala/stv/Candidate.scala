package stv
import scala.util.Random

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
    Election(candidates, ranking :: votes.toList, seats)
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
    val excluded = eliminated ++ Set(selected)
    vote.filter(other => !excluded.contains(other)) match {
      case Nil => None
      case head :: tail => Some(AllocatedVote(vote, head))
    }
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
 * An intermediate or final election result.
 * @param elected List of elected candidates, starting with the most recent
 * @param hopefuls Set of hopeful candidates
 * @param eliminated Set of eliminated candidates
 */
final case class ElectionResult(elected: List[ElectedCandidate] = List(), hopefuls: Set[HopefulCandidate] = Set(), eliminated: Set[EliminatedCandidate] = Set()) {
  require(elected.nonEmpty || hopefuls.nonEmpty || eliminated.nonEmpty)

  def isFinal = hopefuls.isEmpty
}


/**
 * A count determines the result of an election.
 *
 * Counting methods differ in how the quota is determined and how unused votes
 * may be transferred.
 *
 * A wasted vote is a vote that cannot be allocated to a candidate,
 * because all expressed preferences have been elected already or
 * eliminated. These votes are simply discarded.
 * STV counting methods aim to minimise these by transferring the vote to the
 * next preferred candidate whenever possible.
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
   * Reallocate a random number of surplus votes from a single elected candidate
   */
  def reallocateRandomSurplus(donor: ElectedCandidate, ignore: Set[Candidate]): List[AllocatedVote] = {
    val surplus = donor.votes.length - quota
    reallocateSurplusVotes(Random.shuffle(donor.votes).take(surplus), ignore)
  }

  /**
   * Reallocate a set of votes, ignoring already eliminated candidates,
   * and dropping wasted votes.
   * @param votes Surplus votes to be reallocated
   * @param ignore Candidates that shouldn't receive new votes
   * @return A new list of allocated votes, of length equal to or less than the original list
   */
  def reallocateSurplusVotes(votes: List[AllocatedVote], ignore: Set[Candidate]): List[AllocatedVote] = {
    votes.map(_.transfer(ignore)).collect {
      case Some(allocatedVote) => allocatedVote
    }
  }

  /**
   * Reallocate surplus votes of elected candidates one by one, ignoring prior winners and eliminated candidates
   * I think this is the Hare method (???)
   * Ignoring prior winners means this function will eventually terminate, because we will just throw away
   * votes that cannot be transferred.
   * @param initial
   * @return A new election result after reallocation has taken place
   */
  def reallocateAllSurplus(initial: ElectionResult): ElectionResult = {
    val donors = initial.elected.filter(_.votes.length > quota)
    donors match {
      case Nil => initial
      case donor :: tail => {
        val ignore = initial.eliminated.map(_.candidate) ++ initial.elected.map(_.candidate).toSet
        val reallocatedVotes = reallocateRandomSurplus(donor, ignore)
        val newResult = countAllocation(reallocatedVotes, initial.eliminated)
        reallocateAllSurplus(newResult)
      }
    }
  }

  /**
   * Find new elected candidates from the hopeful set.
   * @param hopefuls Set of unelected candidates
   * @return Partition of Elected/Hopeful candidates
   */
  def repartition(hopefuls: Set[HopefulCandidate]): Tuple2[List[ElectedCandidate], Set[HopefulCandidate]] = {
    val (elected, unelected) = hopefuls.partition(hopeful => meetsQuota(hopeful.votes))
    (
      elected.map(hopeful => ElectedCandidate(hopeful.candidate, hopeful.votes)).toList,
      unelected
    )
  }

  /**
   * Partition a set of allocated votes into Elected/Unelected lists
   * @param votes All allocated votes
   * @return Tuple of elected list and hopeful list
   */
  def partition(votes: List[AllocatedVote]): Tuple2[List[ElectedCandidate], Set[HopefulCandidate]] = {
    val hopefuls = votes.groupBy(_.selected).map(group => HopefulCandidate(group._1, group._2))
    repartition(hopefuls.toSet)
  }

  /**
   * Count an allocation of votes.
   * @param votes All allocated votes
   * @param eliminated All eliminated candidates
   * @return The current election result given this vote allocation
   */
  def countAllocation(votes: List[AllocatedVote], eliminated: Set[EliminatedCandidate]) = {
    val (elected, hopeful) = partition(votes)
    ElectionResult(elected, hopeful, eliminated)
  }

  /**
   * Eliminate a single candidate.
   * @param currentResult the current election result
   * @return The new result
   */
  def eliminate(currentResult: ElectionResult): ElectionResult = {
    require(currentResult.hopefuls.nonEmpty)

    val eliminated = currentResult.eliminated
    val elected = currentResult.elected
    val hopefuls = currentResult.hopefuls
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

    val newEliminated = eliminated + EliminatedCandidate(loser.candidate)
    val (newElected, stillHopeful) = repartition(
      for (
        hopeful <- hopefuls
        if hopeful != loser
      ) yield (HopefulCandidate(hopeful.candidate, hopeful.votes ++ loserVotesByCandidate(hopeful.candidate)))
    )

    ElectionResult(newElected ++ stillElected, stillHopeful, newEliminated)
  }

  /**
   * Count the election.
   * @return A list containing the results per candidate at the end of each
   *         round.
   */
  def roundCounts: List[ElectionResult] = {
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
    val (firstChoiceElected, firstChoiceUnelected) = firstChoiceVotes.partition(
      _._2.length > quota
    )

    val electedCandidates = for (
      (candidate: Candidate, votes: List[AllocatedVote]) <- firstChoiceElected
    ) yield ElectedCandidate(candidate, votes)

    val hopefulCandidates = for (
      (candidate: Candidate, votes: List[AllocatedVote]) <- firstChoiceUnelected
    ) yield HopefulCandidate(candidate, votes)

    val initialResults = ElectionResult(electedCandidates.toList, hopefulCandidates.toSet)

    /**
     *
      In each round of counting, we either:
      1) Transfer surplus votes from newly elected candidates
      2) Eliminate candidate and transfer their votes
      We only eliminate when no new candidates can be elected through step 1.
     * @param currentRound The results as they stand so far.
     * @return The results after running a new round of counting.
     */
    def nextRound(currentRound: ElectionResult): ElectionResult = {
      val reallocatedSurplusResults = reallocateAllSurplus(currentRound)
      val newlyElected = reallocatedSurplusResults.elected.length - currentRound.elected.length > 0

      if(newlyElected) {
        reallocatedSurplusResults
      } else {
        eliminate(reallocatedSurplusResults)
      }
    }

    var rounds: List[ElectionResult] = List(initialResults)
    var currentRound = initialResults
    while(!currentRound.isFinal) {
      currentRound = nextRound(currentRound)
      rounds = rounds :+ currentRound
    }

    rounds
  }

  /**
   * Count the election.
   * @return The results per candidate after the vote is counted.
   */
  def finalCount: ElectionResult = roundCounts.last

  /**
   * Determine who wins each seat.
   * @return The candidates who fill each seat after the vote is counted.
   */
  def finalSeats: List[Option[ElectedCandidate]] = {
    val filledSeats: List[Some[ElectedCandidate]] = finalCount.elected.take(numSeats).map(Some(_))
    filledSeats ++ List.fill(numSeats - filledSeats.length) {None}
  }
}

case class SomeCount(election: Election) extends Count {
  val quota = droopQuota
}

object Test extends App {
  val foo = Candidate("Foo")
  val bar = Candidate("Bar")
  var election = new Election(Set(foo, bar))
  election = election.vote(List(foo, bar))
  println(election.votes)
}