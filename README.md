# NBA Party

### The question
It's not unreasonable to think that game performance can be affected (in part) by what takes place off the court. We are interested to see if teams from quiet cities exhibit a decline in performance the day following a game in a party city. We exploit data on bookmaker spreads, the expected score differential between two teams after conditioning on observable performance. We expect a team to meet the spread half the time, since this is how bookmakers minimize risk on their end. We construct a model which estimates the causal effect of  visiting a party-city the day before a game on the probability of meeting the spread.

### The set up
We define a *party city* to be one of the following: Atlanta, Brooklyn, Los Angeles, or Miami. We then look for other teams (not from one of these cities)
who travel to a party city and then play a game in another city the day after.

### Confounders
What if we pick up the effect of a busy play-schedule? We use a difference in difference model.

- Compare the difference in performance when a team has visited a party city the day before to the difference in performance when a team has visited any other city the day before.

Model sketch
$$
\texttt{meet spread} \sim \substack{\texttt{days since} \\ \texttt{last game}} + \substack{\texttt{games played} \\ \texttt{last week}} + \substack{\texttt{team} \\ \texttt{ranking}} + \texttt{party}
$$

### References
"The Benefits of College Athletic Success: An Application of the Propensity Score Design", by Michael L. Anderson. March, 2017. [Paper](http://www.mitpressjournals.org/doi/pdfplus/10.1162/REST_a_00589)