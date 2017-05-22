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

### Anecdotes
"There's no nightlife in Utah", May 2017. [Article](http://kron4.com/2017/05/01/warriors-forward-matt-barnes-on-playing-jazz-in-second-round-of-nba-playoffs-theres-no-nightlife-in-utah/)

### References
"The Benefits of College Athletic Success: An Application of the Propensity Score Design", by Michael L. Anderson. March, 2017. [Paper](http://www.mitpressjournals.org/doi/pdfplus/10.1162/REST_a_00589)

"Jet Lag in Athletes", May 2012. [Paper](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3435929/)

"Jet lag puts baseball players off their game", Jan 2017.
[Article](https://www.sciencemag.org/news/2017/01/jet-lag-puts-baseball-players-their-game)

"Injury in the National Basketball Association", by Drakos et. al. July, 2010. [Paper](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3445097/)

"How to read a baseball betting line", June 2007. [Paper](http://bleacherreport.com/articles/1113-how-to-read-a-baseball-betting-line)

"MLB to Baseball Teams: Stop Celebrating in Public", Oct 2015.[Paper](http://www.esquire.com/sports/news/a39092/mlb-players-spraying-champagne-partying-rules/)

"The home field advantage in athletics: a meta-analysis", Jul 2010. [Paper](http://onlinelibrary.wiley.com/doi/10.1111/j.1559-1816.2010.00641.x/full)

"An analysis of the home-field advantage in major league baseball using logit models: evidence from the 2004 and 2005 seasons", January 2007. [Paper](https://www.degruyter.com/view/j/jqas.2007.3.1/jqas.2007.3.1.1045/jqas.2007.3.1.1045.xml?format=INT)

"A starting point for analyzing basketball statistics", July 2007. [Paper](https://www.degruyter.com/view/j/jqas.2007.3.3/jqas.2007.3.3.1070/jqas.2007.3.3.1070.xml?format=INT)

"Home advantage in the NBA as a game-long process", October 2007. [Paper](https://www.degruyter.com/view/j/jqas.2007.3.4/jqas.2007.3.4.1081/jqas.2007.3.4.1081.xml?format=INT)

"A note on the team-specific home advantage in the NBA", July 2008. [Paper](https://www.degruyter.com/view/j/jqas.2008.4.3/jqas.2008.4.3.1128/jqas.2008.4.3.1128.xml?format=INT)

"Estimated age effects in baseball", January 2008. [Paper](https://www.degruyter.com/view/j/jqas.2008.4.1/jqas.2008.4.1.1074/jqas.2008.4.1.1074.xml?format=INT)

"Racial bias in the NBA: implications in betting markets", April 2008. [Paper](https://www.degruyter.com/view/j/jqas.2008.4.2/jqas.2008.4.2.1112/jqas.2008.4.2.1112.xml?format=INT)

"Predicting NBA games using neural networks", January 2009. [Paper](https://www.degruyter.com/view/j/jqas.2009.5.1/jqas.2009.5.1.1156/jqas.2009.5.1.1156.xml?format=INT)

"Scoring and shooting abilities of NBA players", January 2010. [Paper](https://www.degruyter.com/view/j/jqas.2010.6.1/jqas.2010.6.1.1194/jqas.2010.6.1.1194.xml?format=INT)

"Do fans matter? The effect of attendance on the outcomes of major league baseball games", January 2010. [Paper](https://www.degruyter.com/view/j/jqas.2010.6.1/jqas.2010.6.1.1192/jqas.2010.6.1.1192.xml?format=INT)

"Relative Importance of Performance Factors in Winning NBA Games in Regular Season versus Playoffs", July 2010. [Paper](https://www.degruyter.com/view/j/jqas.2010.6.3/jqas.2010.6.3.1260/jqas.2010.6.3.1260.xml?format=INT)

"Adjusting Winning-Percentage Standard Deviations and a Measure of Competitive Balance for Home Advantage", January 2011. [Paper](https://www.degruyter.com/view/j/jqas.2011.7.1/jqas.2011.7.1.1297/jqas.2011.7.1.1297.xml?format=INT)

"Stratified Odds Ratios for Evaluating NBA Players Based on their Plus/Minus Statistics", May 2011. [Paper](https://www.degruyter.com/view/j/jqas.2011.7.2/jqas.2011.7.2.1320/jqas.2011.7.2.1320.xml?format=INT)

"Dependence Relationships between On Field Performance, Wins, and Payroll in Major League Baseball", May 2011. [Paper](https://www.degruyter.com/view/j/jqas.2011.7.2/jqas.2011.7.2.1321/jqas.2011.7.2.1321.xml?format=INT)

"On Estimating the Ability of NBA Players", July 2011. [Paper](https://www.degruyter.com/view/j/jqas.2011.7.3/jqas.2011.7.3.1298/jqas.2011.7.3.1298.xml?format=INT)

"Prediction Accuracy of Linear Models for Paired Comparisons in Sports", July 2011.[Paper](https://www.degruyter.com/view/j/jqas.2011.7.3/jqas.2011.7.3.1303/jqas.2011.7.3.1303.xml?format=INT)

"Random Walk Picture of Basketball Scoring", March 2012. [Paper](https://www.degruyter.com/view/j/jqas.2012.8.issue-1/1559-0410.1416/1559-0410.1416.xml?format=INT)

"Modeling and forecasting the outcomes of NBA basketball games", January 2016. [Paper](https://www.degruyter.com/view/j/jqas.2016.12.issue-1/jqas-2015-0088/jqas-2015-0088.xml?format=INT)

"Estimating an NBA player’s impact on his team’s chances of winning", March 2016. [Paper](https://www.degruyter.com/view/j/jqas.2016.12.issue-2/jqas-2015-0027/jqas-2015-0027.xml?format=INT)

"Meta-analytics: tools for understanding the statistical properties of sports metrics", March 2017. [Paper](https://www.degruyter.com/view/j/jqas.2016.12.issue-4/jqas-2016-0098/jqas-2016-0098.xml?format=INT)

"Why can't baseball players act more like hockey players", Oct 2015. [Paper](http://www.thehockeynews.com/news/article/why-cant-baseball-players-act-more-like-hockey-players)

### R packages
 Hlavac, Marek (2015). stargazer: Well-Formatted Regression and Summary Statistics Tables.
 R package version 5.2. [Package](http://CRAN.R-project.org/package=stargazer )
