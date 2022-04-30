# MOVIE RECOMMENDER SYSTEM

# install required packages 
install.packages("dplyr", "arules" , "Matrix" , "arulesViz" , "ggplot2")
library(dplyr, arules, Matrix, arulesViz, ggplot2)

# read files 
movies <- read.csv(file="C:/Users/user/Desktop/Recommender/movies.csv", header = TRUE,stringsAsFactors=FALSE)
ratings <- read.csv(file="C:/Users/user/Desktop/Recommender/ratings.csv", header = TRUE)

# merge into one table / reorganize
movies2 <- merge(movies,ratings, by= "movieId")
movie_data <- movies2 %>% select("movieId", "userId", "genres", "title")

# data understanding
n_distinct(movie_data$movieId)
n_distinct(movie_data$userId)
n_distinct(movie_data$movieId) /n_distinct(movie_data$userId)

movie_data %>% 
    group_by(userId) %>% 
    summarize(nb_movies = n_distinct(movieId)) %>%
    ggplot(aes(x = nb_movies)) +
    geom_bar(width = 1) + xlim(10,500) + ggtitle("Distribution of number of movies watched")

# turn dataset into transactional class
movie_list <- split(movie_data$title, movie_data$userId)
movie_trx <- as(movie_list, "transactions")

image(movie_trx[1:100,1:100])
summary(movie_trx)

# most and least frequent movies
itemFrequencyPlot(movie_trx, type = "absolute", topN = 10, horiz = TRUE , main = "Absolute Item Frequency", col=rainbow(10))

# picking the right movie parameters
  # calculate the number of rules extracted
confidenceLevels = seq(from=0.95 , to=0.5 , by= -0.05)
  # create empty vector
rules_sup04 = NULL
  #Apriori algorithm with a support level of 40%
for ( i in 1:length(confidenceLevels)) {
                                        rules_sup04[i] = 
                                        length(apriori(movie_trx,
                                        parameter = list(sup=0.4,
                                        conf=confidenceLevels[i],
                                        target = "rules")))
                                        }
 # create rules_sup02 and rules_sup03 in the same way to decide which support level covers the minimum threshold

# Create data frame with all metrics to be plotted
nb_rules_2 = data.frame(rules_sup04, rules_sup03, rules_sup02, confidenceLevels)

# Plot the number of rules as a function of the confidence level
ggplot(data=nb_rules_2, aes(x=confidenceLevels)) +
     geom_line(aes(y=rules_sup04, colour="Support level of 40%")) + 
     geom_point(aes(y=rules_sup04,colour="Support level of 40%")) +
     geom_line(aes(y=rules_sup03, colour="Support level of 30%")) +
     geom_point(aes(y=rules_sup03,colour="Support level of 30%")) +
     geom_line(aes(y=rules_sup02, colour="Support level of 20%")) + 
     geom_point(aes(y=rules_sup02,colour="Support level of 20%")) +
     theme_bw() + ylab("") +
     ggtitle("Number of extracted rules with apriori")

# extract rules, rules_movies
rules_movies <- apriori(movie_trx, parameter = list(supp= 0.3, conf = 0.9, minlen = 2, target = "rules"))

# finding the redundant rules
rules_redundant <- is.redundant(rules_movies)
rules_pruned <- rules_movies[!rules_redundant]
inspect(head(sort(rules_pruned, by = "confidence"),5))
  # we have 7 total rules
rules_movies
  # no redundant rules
rules_redundant

# which watched movies are most likely to lead to a recommendation of the movie Pulp Fiction?
pulpfiction_rules_rhs <- apriori(movie_trx,  
                                 parameter = list(supp= 0.3, conf = 0.7), 
                                 appearance = list(default= "lhs", rhs = "Pulp Fiction (1994)"))

inspect(head(pulpfiction_rules_rhs))

# Which movie should I watch after Pulp Fiction?
pulpfiction_rules_lhs <- apriori(movie_trx, 
                                 parameter = list(supp=0.3, conf = 0.7), 
                                 appearance = list( default = "rhs", lhs = "Pulp Fiction (1994)"))

inspect(head(pulpfiction_rules_lhs))




