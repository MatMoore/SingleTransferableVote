# Single Transferable Vote

Scala package to count [Single Transferable Vote](https://en.wikipedia.org/wiki/Single_transferable_vote) elections.

This is still a work in progress. I'm using this project to practice Scala.

To actually run elections, try http://www.openstv.org

## What is STV?
Single transferable vote (STV) is a set of voting systems for choosing a number of candidates based on people's preferences.

* Voters fill out a ballot by ranking candidates in order of preference
* A quota of votes is required to elect a candidate to a seat
* To start with, only the first choice votes of each ballot are considered
* The least popular candidates are eliminated one by one
* If a ballot is wasted on the first choice, the vote is transferred to the second choice; if it's wasted on the second, then the third choice and so on.

## Similarities to other voting systems

* If you ignore all preferences except the first choice, you get the first past the post system.
* If you only need to select one winner, you get Instant Runoff Voting (IRV), where votes are only transferred when candidates are eliminated, and the counting stops as soon as any one candidate has enough votes. STV methods continue until all seats are filled, and can also transfer votes when there are extra votes for an already elected candidate.
