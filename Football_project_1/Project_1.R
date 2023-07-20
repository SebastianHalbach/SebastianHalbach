Football_stats <- read.csv("Football_stats.csv")

library(ggplot2)
install.packages("rvest")
library(rvest)

Football_stats$Player_last_name <- c("Neymar", "Mbappe", "Coutinho", 
                                     "Dembele", "Felix", "Griezmann", 
                                     "Grealish", "Ronaldo", "Hazard", "Pogba")

Football_stats$League_transfered_to <- c("League_1", "League_1", "La_Liga", 
                                         "La_Liga", "La_Liga", "La_Liga", "Premier_League", 
                                         "Serie_A", "La_Liga", "Premier_League")


# Convert the columns to the correct data types
Football_stats$Player_last_name <- factor(Football_stats$Player_last_name)
Football_stats$League_transfered_to <- factor(Football_stats$League_transfered_to)

# Create the ggplot
ggplot(Football_stats, aes(x = Price_._Million, y = Goals_and_assists_both_seasons)) +
  geom_point(aes(fill = League_transfered_to), shape = 21, color = "black", size = 3) +
  ylim(0, max(100)) +
  xlim(80, max(250)) +
  labs(x = "Price (€Million)", y = "Goals and Assists (first two seasons)") +
  geom_text(aes(label = Player_last_name), vjust = -1, size = 3) +
  theme_minimal() + 
  geom_smooth(method='lm')

# Run linear model of g/a both season against price
Goals_and_assissts_linear_reg <- lm(Football_stats$Goals_and_assists_both_seasons ~ Football_stats$Price_._Million)

# Find the residuals (the distance between the observations and the line of best fit)
Football_stats$residuals_goals_assists_both_seasons <- Goals_and_assissts_linear_reg$residuals



# Line of best fit, and see distance from points to line
