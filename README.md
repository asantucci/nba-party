# nba-party

## The question
It's not unreasonable to think that game performance can be affected (in part) by what takes place off the court. We are interested to see if teams from quiet cities exhibit a decline in performance the day following a game in a party city. We exploit data on bookmaker spreads, the expected score differential between two teams after conditioning on observable performance, as a way to estimate the probability of winning each game. We then condition on these probabilities to estimate the causal effect of visiting a party-city the day before a game. 

### The set up
We define a *party city* to be one of the following,

* *Party Cities*: Los Angeles, Miami, or Brooklyn.

We look for other teams
- Look for other teams who travel to one of the above party cities, and then also play a game in *another* city on the following day.
- If we look at performance against the spread in the following game, we can suss out a causal effect.
