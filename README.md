# nba-party

## The question
It's not unreasonable to think that game performance can be affected (in part) by what takes place off the court. We are interested to see if teams from quiet cities exhibit a decline in performance the day following a game in a party city. We exploit data on bookmaker spreads, the expected score differential between two teams after conditioning on observable performance, as a way to estimate the probability of winning each game. We then condition on these probabilities to estimate the causal effect of visiting a party-city the day before a game. 

### The set up
We define a *party city* to be one of the following: Los Angeles, Miami, or Brooklyn. We then look for other teams (not from one of these cities)
who travel to a party city and then play a game in another city the day after.

### Confounders
What if we pick up the effect of a busy play-schedule? We use a difference in difference model.

- Compare the difference in performance when a team has visited a party city the day before to the difference in performance when a team has visited any other city the day before.
