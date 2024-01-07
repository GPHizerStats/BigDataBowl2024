# Houston, We Have a (Tackling) Problem: An Analysis of Potential Acquisitions for the Houston Texans
### Appendix and Code for our 2024 NFL Big Data Bowl Submission <br> *Nick Fullerton, Grant Hizer, and CJ Olson*

For our Big Data Bowl submission we pursued the coaching presentation track, identifying poor and sound tacklers across the NFL using a series of expected tackling metrics that we developed. After identifying the Texans as one of the poorest tackling teams in the NFL during the 2022 NFL season, we evalated areas they may improve and players they could target via trade/free agency to help increase the team's tackling productivity as a whole.

## Data Cleaning and Modeling
To prepare our data we conducted explorartory data analysis and feature testing to figure out which variables to include in our expected tackle percentage model. We narrowed the given tracking data down to all defensive players on a given play and included: 
* The defender's location on the field, speed, and direction he is moving
* The ball carrier's location on the field, speed, and direction he is moving
* The distance between the ball carrier and defender
* The average tackle rate against the ball carrier
* Whether the play is a pass or rush
After trimming down the tracking data to these features, it was time to create our model.

For our model we used a machine learning technique called Gradient Boosting, which focuses on the results of weak learners/poor results within the modeling process and takes a series of steps to "fix" those week learners, finally averaging all models to ideally create a more accurate model. Specifically we chose gradient boosted trees using the "lightgbm" engine within R, as we found that it has been shown to increase speed and memory-efficiency during the modeling process, as compared to xgBoost. Additionally, we implemented cross-validation across our dataset to generate less biased results.

## Analysis
After creating our models, our goal then shifted to making the results as understandable for your average coach or personnel executive as possible. To make our data more palatable, we ranked all qualified (min. 10 defensive snaps per game) defenders to in each of our 4 primary metrics:
* TPOE ~ Tackle % Over Expected
* TOE ~ Tackles Over Expected
* MTPOE ~ Missed Tackle % Over Expected
* MTOE ~ Missed Tackles Over Expected
From here, we additionally made a "Composite Tackle Ranking" as an average of the 4 ranks above. The master list that was exported back into R can be found [here](https://docs.google.com/spreadsheets/d/1C7r6EZzI5RnjAnbmmlIOpANZLml9QrbyWd0Dw7k2yZQ/edit?usp=sharing), with the file already in the "projectData" folder. Finally, evaluated the contract structure of all qualified players to help us hone in on "gettable" players, who we then evaluated to see if they would be a good fit according to our metrics.

## Visualizations
We implemented two primary forms of visualizations for this project. All of the tables seen we developed via the gt/gtExtras package in R, with data being supplemented in from the nflfastR libraries. The field visualizations were made using the ggplot2 library, as well as a supplementary function titled "gg_field", created and publically shared by statistician [Marschall Fulman](https://github.com/mlfurman3/gg_field).

Script Order for running our code[^1]:
1. createMetricModels.R
2. createMetricLeaderboards.R
3. createTableVisualizations.R
4. gg_field.R
5. createFieldVisualizations.R

[^1]: All necessary data files to run the scripts are included except the tracking data for each week provided by the big data bowl, which were too big to include
