# Loading libraries
rm(list = ls())
library(ggplot2)
library(cowplot)

# Reading the datasets
d1 <- read.csv("Ludo_Monthly_Data.csv")[, -1]
d2 <- read.csv("Ludo_Game_Win_Count.csv")[, -1]

# Preparing a base data frame for matching and merging
d3 <- as.data.frame(cbind(d1[, 1], 0, 0))
for (i in 1:nrow(d3)) {
  d3[i, -1] <- d2[which(d2 == d3[i, 1]), -1]
}

# Computing deciles for total games played
quan <- quantile(d3[, 2], seq(0, 1, 0.1))

# Reading cleaned data and filtering relevant rows
d4 <- read.csv("Ludo_Cleaned_Data.csv")[, -(1:2)]
common <- d3[, 1]
d5 <- d4[intersect(which(d4[, 1] %in% common), which(d4[, 2] %in% common)), ]
dummy <- d5[, 1] == d5[, 3]
d5 <- as.data.frame(cbind(d5, dummy))

# Initializing matrices for results based on deciles
dec_mat <- matrix(0, nrow = 10, ncol = 10)
dec_game <- matrix(0, nrow = 10, ncol = 10)

# Filling in decile-based mean and counts
for (i in 2:10) 
 {
  for (j in 2:10) 
    {
    dec_mat[i, j] <- mean(d5[intersect(
      intersect(which(d5[, 4] > quan[i - 1]), which(d5[, 4] <= quan[i])),
      intersect(which(d5[, 5] > quan[j - 1]), which(d5[, 5] <= quan[j]))
    ), 6])
    dec_game[i, j] <- nrow(d5[intersect(
      intersect(which(d5[, 4] > quan[i - 1]), which(d5[, 4] <= quan[i])),
      intersect(which(d5[, 5] > quan[j - 1]), which(d5[, 5] <= quan[j]))
    ), ])
    }
 }

for (i in 1:10) 
{
  dec_mat[i, 1] <- mean(d5[intersect(
    intersect(which(d5[, 4] > quan[i - 1]), which(d5[, 4] <= quan[i])),
    which(d5[, 5] <= quan[1])
  ), 6])
  dec_game[i, 1] <- nrow(d5[intersect(
    intersect(which(d5[, 4] > quan[i - 1]), which(d5[, 4] <= quan[i])),
    which(d5[, 5] <= quan[1])
  ), ])
}

dec_mat[1, 1] <- mean(d5[intersect(which(d5[, 4] <= quan[1]), which(d5[, 5] <= quan[1])), 6])
dec_game[1, 1] <- nrow(d5[intersect(which(d5[, 4] <= quan[1]), which(d5[, 5] <= quan[1])), ])
dec_mat <- round(dec_mat, 3)
dec_game <- round(dec_game, 3)

# Calculating win proportions and dispersion metrics for deciles
d3[, 3] <- d3[, 3] / d3[, 2]
dec_win_summ <- numeric(30)
dec_sd_iqr <- numeric(20)
for (i in 1:10) {
  temp <- d3[intersect(which(d3[, 2] > quan[i]), which(d3[, 2] <= quan[i + 1])), 3]
  dec_win_summ[3 * i - 2:3 * i] <- c(quantile(temp, 0.025), mean(temp), quantile(temp, 0.975))
  dec_sd_iqr[2 * i - 1:2 * i] <- c(sd(temp), IQR(temp))
}

# Creating scatterplots for analysis
scatter_plot1 <- ggplot(d3, aes(x = log(d3[, 2]), y = d3[, 3])) +
  geom_point(color = "blue") +
  labs(
    x = "log(Total No. of Games Played)", y = "Win Proportion",
    title = "Scatterplot of Win Proportion vs Total Games"
  ) +
  theme_minimal()

scatter_plot2 <- ggplot(data.frame(
  x = rep(1:10, each = 3),
  y = dec_win_summ,
  group = factor(rep(c("2.5th Quantile", "Mean", "97.5th Quantile"), 10)),
  color = rep(c("blue", "purple", "brown"), 10)
), aes(x = x, y = y, color = color)) +
  geom_point(size = 3) +
  scale_color_identity() +
  labs(title = "Win Proportion in Each Decile", x = "Decile", y = "Win Proportion") +
  theme_minimal()

scatter_plot3 <- ggplot(data.frame(
  x = rep(1:10, each = 2),
  y = dec_sd_iqr,
  group = factor(rep(c("Standard Deviation", "IQR"), 10)),
  color = rep(c("purple", "blue"), 10)
), aes(x = x, y = y, color = color)) +
  geom_point(size = 3) +
  scale_color_identity() +
  labs(title = "Dispersion of Win Proportion", x = "Decile", y = "Dispersion Measure") +
  theme_minimal()

# Displaying plots
scatter_plot1
scatter_plot2
scatter_plot3
