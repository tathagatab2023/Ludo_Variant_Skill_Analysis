
rm(list = ls())

# Reading data
d = read.csv("Simulated_Data.csv")[,-1]

# Function for updating player ratings using the Elo formula
ratings_update = function(arr, w1, w2, r1, r2, K1, K2) {
  p1 = 1 / (1 + 10^((r2 - r1) / 400))
  p2 = 1 / (1 + 10^((r1 - r2) / 400))
  
  s1 = arr[3]
  s2 = 1 - arr[3]
  
  r11 = r1 + round(K1 * (s1 - p1))
  r21 = r2 + round(K2 * (s2 - p2))
  return(c(r11, r21))
}

# Function for determining K-factor based on games played
k_det = function(games) {
  if (games <= 5) {
    k = 60
  } else if (games <= 10) {
    k = 30
  } else {
    k = 16
  }
  return(k)
}

# Selecting relevant columns and order data by timestamp
d = d[, c(1, 3, 5, 6, 7, 8, 11)]
ts_ordered = order(d$timestamp)
df = d[ts_ordered,]

# Filtering out invalid or inconsistent game entries
df = df[which(df$player_current_score + df$opponent_current_score > 0),]
df = df[-intersect(which(df$net_winnings < 0), which(df$player_current_score > df$opponent_current_score)),]
df = df[-intersect(which(df$player_current_score == 0), which(df$user_move_rank == 2)),]
df = df[-intersect(which(df$opponent_current_score == 0), which(df$user_move_rank == 1)),]

# Adding 'winner' column based on scores and rearranging columns
winner = as.numeric(df$player_current_score > df$opponent_current_score + 0.5 * (df$player_current_score == df$opponent_current_score))
df = cbind(df, winner)

df = cbind(df[, 1:3], df$winner, df[, -c(1, 2, 3, 8)])
colnames(df)[4] = "winner"

# Separating data into appropriate subsets, can be skipped if no subsetting required
di = d[1:60000,]
dci = d[c(30001:40000, 50001:60000, 60001:64000),]
dc = d[64001:184000,]

# Function to calculate the number of games played by each player
gamecount = function(df) {
  p1_games = numeric(length = nrow(df))
  p2_games = p1_games
  colnames(df) = c("Sl.", "user_id", "opponent_user_id", "Winner")
  users = unique(c(df[, 2], df[, 3]))
  
  for (i in 1:length(users)) {
    t = matrix(which(df[, 2:3] == users[i], arr.ind = TRUE), ncol = 2)
    if (nrow(t) > 1) {
      t = t[order(t[, 1]),]
    }
    t1 = t[which(t[, 2] == 1), 1]
    t2 = t[which(t[, 2] == 2), 1]
    p1_games[t1] = which(t[, 2] == 1)
    p2_games[t2] = which(t[, 2] == 2)
    
    if (i %% 1000 == 0)
      print(i)
  }
  return(cbind(p1_games, p2_games))
}

# Calculating games played for each subset
s = gamecount(di)
s1 = gamecount(dc)
s2 = gamecount(dci)

di = cbind(di, s)
dc = cbind(dc, s1)
dci = cbind(dci, s2)

# Function for calculating dynamic Elo ratings
rating = function(df) {
  colnames(df)[2:3] = c("user_id", "opponent_user_id")
  users = sort(unique(c(df$user_id, df$opponent_user_id)))
  
  temp = c(df$user_id, df$opponent_user_id)
  freq = table(temp)
  
  users_track = rep(users, freq)
  d1 = data.frame(users_track, 1000)
  colnames(d1) = c("Player", "Rating")
  
  d = df
  
  for (i in 1:nrow(d)) {
    arr = d[i, -1]
    loc1 = which(d1$Player == as.character(arr[1]))
    loc2 = which(d1$Player == as.character(arr[2]))
    t1 = loc1[unlist(arr[4])]
    t2 = loc2[unlist(arr[5])]
    r1 = ifelse(arr[4] == 1, 1000, d1[loc1[unlist(arr[4]) - 1], 2])
    r2 = ifelse(arr[5] == 1, 1000, d1[loc2[unlist(arr[5]) - 1], 2])
    
    K1 = k_det(arr[4])
    K2 = k_det(arr[5])
    
    r = ratings_update(arr, w1, w2, r1, r2, K1, K2)
    d1[t1, 2] = r[1]
    d1[t2, 2] = r[2]
    
    if (i %% 1000 == 0)
      print(i)
  }
  return(d1)
}

# Calculating ratings for one subset, can be configured
t = rating(di)

# Logistic regression for modelling the probability of a win
t1 = glm(dreg$winner ~ dreg$p1_rating + dreg$p2_rating, family = "binomial")
