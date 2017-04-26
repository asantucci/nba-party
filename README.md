# NBA Party

### The question
It's not unreasonable to think that game performance can be affected (in part) by what takes place off the court. We are interested to see if teams exhibit a decline in performance the day following a game in a party city. We exploit data on bookmaker spreads, the expected score differential between two teams after conditioning on observable performance. We expect a team to meet the spread half the time, since this is how bookmakers minimize risk on their end. We construct a model which attempts to estimate the causal effect of  visiting a "party-city" the day before a game on the probability of meeting the spread.

### The set up
We define a *party city* to be one of the following: Atlanta, Brooklyn, Los Angeles, or Miami. We then look for other teams (not from one of these cities)
who travel to a party city and then play a game in another city the day after; each of the thirty NBA teams encounters this situation on average twice per season.

### Confounders
What if we pick up the effect of a busy play-schedule, or travel from a far-away city? We consider a difference in difference model: compare the difference in performance when a team has visited a party city the day before to the difference in performance when a team has visited any other city the day before. Model sketch:
$$
\texttt{meet spread} \sim \substack{\texttt{days since} \\ \texttt{last game}} + \substack{\texttt{games played} \\ \texttt{last week}} + \substack{\texttt{team} \\ \texttt{ranking}} + \substack{\texttt{time}\\ \texttt{difference}} + \substack{\texttt{distance} \\ \texttt{traveled}} + \texttt{party}
$$

We note that some of these predictors might have non-linear effects, e.g. distance traveled may only become important when it passes some threshold. Others, such as number of games played "recently", may benefit from some type of kernel.

### Data Sources

### References
"The Benefits of College Athletic Success: An Application of the Propensity Score Design", by Michael L. Anderson. March, 2017. [Paper](http://www.mitpressjournals.org/doi/pdfplus/10.1162/REST_a_00589)

"Injury in the National Basketball Association", by Drakos et. al. July, 2010. [Paper](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3445097/)

"How to read a baseball betting line", June 2007. [Paper](http://bleacherreport.com/articles/1113-how-to-read-a-baseball-betting-line)

"MLB to Baseball Teams: Stop Celebrating in Public", Oct 2015.[Paper](http://www.esquire.com/sports/news/a39092/mlb-players-spraying-champagne-partying-rules/)

"The home field advantage in athletics: a meta-analysis", Jul 2010. [Paper](http://onlinelibrary.wiley.com/doi/10.1111/j.1559-1816.2010.00641.x/full)

"Why can't baseball players act more like hockey players", Oct 2015. [Paper](http://www.thehockeynews.com/news/article/why-cant-baseball-players-act-more-like-hockey-players)