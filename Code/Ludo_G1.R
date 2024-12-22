#code for simulation of 3 dice,2 player ludo game
# five skill levels of players are labelled as: 0-unskilled,1-aggressive,2-random,3-tree search skilled,4-responsible pair
library(foreach)
library(doParallel)
library(dplyr)



dice_roll = function(d = 3) #function to roll 3 dice 
{
  return(sample(1:6,size = d,replace=T))
}

n = 16 #number of turns in the game, 16 turns correspond to 24 moves for each player

roll_choice = function(red_store,blue_store,red_skill,blue_skill,p1,roll,move,bonus) #function to choose roll out of the available dice rolls
{
  #print(roll)
  t_roll = sort(roll,decreasing = TRUE)
  if(p1 == 1)
  {
    p1_active = red_store[1:4]
    p2_active = blue_store[1:4]
    p1_index = red_store[5:8]
    p2_index = blue_store[5:8]
    p1_store = red_store
    p2_store = blue_store
    p1_skill = red_skill
    p2_skill = blue_skill
    p1_move = move
    p2_move = move
  }else if(p1 == 2)
  {
    p2_active = red_store[1:4]
    p1_active = blue_store[1:4]
    p2_index = red_store[5:8]
    p1_index = blue_store[5:8]
    p1_store = blue_store
    p2_store = red_store
    p1_skill = blue_skill
    p2_skill = red_skill
    p1_move = move
    p2_move = move + 1
  }
  #active denotes the activity status(0 if token has reached home,1 otherwise, for each player.) index denotes the square the token is presently at(starting sqaure is 1 and home is 57)
  
  element_counts <- table(p2_index)
  unique_elements <- as.numeric(names(element_counts[element_counts == 1]))
  # Identify elements that occur only once for identifying safe zones
  
  token = 0 #token to be moved
  choice = 0 #roll to be chosen
  turn_skip = 0#used to check whether any action has been taken in the turn and skip other actions
  safe = c(1,27,52:57)#default safe squares
  
  if(sum(p1_index == rep(57,4)) == 4 || sum(p2_index == rep(57,4)) == 4)#ending the game if all tokens of a player has reached home
  {
    return(c(p1_active,p1_index,p2_active,p2_index,choice,bonus))
  }
  
  if(sum(is.na(roll)) > 0)#for error handling
  {
    turn_skip = 1
  }
  if(bonus == 4 && roll == 6)
  {
    turn_skip = 1
  }
  if(p1_skill %in% c(2.5,3,3.5) && move == (1.5*n) && turn_skip == 0)
  {
    p1_skill = 2
  }
  if(p1_skill == 2 && turn_skip == 0)#movement for random bot
  {
    temp = c(p1_index + roll[1],p1_index + roll[2],p1_index + roll[3])
    temp[which(temp > 57)] = 0
    temp[which(is.na(temp) == TRUE)] = 0
    if(sum(temp == rep(0,12)) < 12)
    {
      t = sample(which(temp!=0),1)
      token = t %% 4
      choice = roll[ceiling(t / 4)]
      if(token == 0)
        token = 4
      p1_index[token] = p1_index[token] + choice
      turn_skip = 1
      
      for(k in 1:4)
      {
        if(abs(p1_index[token]-p2_index[k])==26 && !(p1_index[token] %in% safe) && !(p2_index[k] %in% safe) && p2_index[k] %in% unique_elements)
        {
          p2_index[k] = 1
          bonus = 2
        }
      }
    }else
    {
      turn_skip = 1
    }
  }
  
  if(p1_skill == 3 && turn_skip == 0 && (move<(1.5*n)))
  {
    #print(roll)
    pt_temp = matrix(0,nrow=4,ncol=length(roll))
    if(length(roll) == 1)
    {
      temp_roll = dice_roll()
    }
    for(i in 1:4)
    {
      for(m in 1:length(roll))
      {
        #print(p1_index[i] + roll[m])
        if((p1_index[i] + roll[m]) <= 57)
        {
          n2 = 100
          
          if(p1 == 2)
          {
            p1_temp = replace(p1_store,(4+i),(p1_index[i] + roll[m]))
            p2_temp = p2_store
            for(k in 1:4)
            {
              if(abs(p1_temp[4 + i]-p2_temp[4+k])==26 && !(p1_temp[4+i] %in% safe) && !(p2_temp[4+k] %in% safe) && p2_temp[4+k] %in% unique_elements)
              {
                p2_temp[4+k] = 1
              }
            }
            if(length(roll) > 1)
            {
              temp_roll = roll[-m]
            }
            pt_temp[i,m] = simulate_game(2,2,c(p2_temp,p1_temp),(move + 1),(move + p1 - 1),temp_roll,1,n2)[(n2+2)]
            #print(c(p2_temp,p1_temp,temp_roll))
          }else
          {
            p1_temp = replace(p1_store,(4+i),(p1_index[i] + roll[m]))
            p2_temp = p2_store
            for(k in 1:4)
            {
              if(abs(p1_temp[4+i]-p2_temp[4+k])==26 && !(p1_temp[4+i] %in% safe) && !(p2_temp[4+k] %in% safe) && p2_temp[4+k] %in% unique_elements)
              {
                p2_temp[4+k] = 1
              }
            }
            
            if(length(roll) > 1)
            {
              temp_roll = roll[-m]
            }
            pt_temp[i,m] = simulate_game(2,2,c(p1_temp,p2_temp),(move + 1),(move + p1 - 1),temp_roll,1,n2)[(n2+1)]
            # print(c(p1_temp,p2_temp,temp_roll))
          }
        }
      }
    }
    #print(pt_temp)
    max_temp = which(pt_temp == max(as.numeric(pt_temp)),arr.ind = TRUE)
    #print(max_temp)
    token = max_temp[1,1]
    p1_index[token] = p1_index[token] + roll[max_temp[1,2]]
    turn_skip = 1
    choice = roll[max_temp[1,2]]
    for(k in 1:4)
    {
      if(abs(p1_index[token]-p2_index[k])==26 && !(p1_index[token] %in% safe) && !(p2_index[k] %in% safe) && p2_index[k] %in% unique_elements)
      {
        p2_index[k] = 1
        bonus = 2
      }
    }
  }
  
  
  
  
  if(p1_skill %in% c(1,4) && turn_skip == 0)#prioritizing promotion for aggressive and responsible pair, it occurs by default for unskilled player
  {
    for(i in 1:4)
    {
      for(m in 1:length(roll))
      {
        if((p1_index[i] + roll[m])== 57)
        {
          p1_index[i] = p1_index[i] + roll[m] #updating index
          token = i
          turn_skip = 1#skipping the turn
          choice = roll[m]#setting choice as the required roll
        }
      }
    }
  }
  
  
  if(p1_skill == 4)#skilled movements for responsible pair
  {
    for(j in 1:4)
    {
      for(k in 1:4)
      {
        if(turn_skip == 0)
        {
          for(m in 1:length(roll))
          {
            if((p1_index[j] + roll[m]) <= 57 && (p1_index[j] > 27) && (p1_index[j] + roll[m]-26) == p2_index[k] && !(p2_index[k] %in% safe) && p2_index[k] %in% unique_elements)
            {#capturing if token is beyond the opponent's starting square
              p1_index[j] = p1_index[j] + roll[m]
              token = j
              p2_index[k] = 1
              choice = roll[m]
              turn_skip = 1
              bonus = 2 #bonus is set as 2 if capture is made
            }else if((p1_index[j] + roll[m]) < 26 && (p1_index[j] + roll[m]+26) == p2_index[k] && !(p2_index[k] %in% safe) && p2_index[k] %in% unique_elements)
            {#capturing if token is before the opponent's starting square
              p1_index[j] = p1_index[j] + roll[m]
              token = j
              p2_index[k] = 1
              choice = roll[m]
              turn_skip = 1
              bonus = 2
            } else if(p1_index[j] < 27 && (p1_index[j] + roll[m]) == 27)
            {#prioritizing moving to 27,which is the opponent's starting square and a safe square
              p1_index[j] = p1_index[j] + roll[m]
              token = j
              choice = roll[m]
              turn_skip = 1
            }else if(p1_index[j] <= 51 && (p1_index[j] + roll[m]) >= 52)
            {#prioritizing moving to last 5 squares,which are safe squares
              p1_index[j] = p1_index[j] + roll[m]
              token = j
              choice = roll[m]
              turn_skip = 1
            } 
          }
        }
      }
    }
  }else if(p1_skill == 1)#skilled movements for aggressive player
  {
    for(j in 1:4)
    {
      for(k in 1:4)
      {
        if(turn_skip == 0)
        {
          for(m in 1:length(roll))
          {
            if((p1_index[j] + roll[m]) < 52 && (p1_index[j] + roll[m]-26) == p2_index[k] && !(p2_index[k] %in% safe) && p2_index[k] %in% unique_elements)
            {#capturing if token is beyond the opponent's starting sqaure
              p1_index[j] = p1_index[j] + roll[m]
              token = j
              p2_index[k] = 1
              choice = roll[m]
              turn_skip = 1
              bonus = 2
            }else if((p1_index[j] + roll[m]) < 26 && (p1_index[j] + roll[m]+26) == p2_index[k] && !(p2_index[k] %in% safe) && p2_index[k] %in% unique_elements)
            {#capturing if token is before the opponent's starting square
              p1_index[j] = p1_index[j] + roll[m]
              token = j
              p2_index[k] = 1
              choice = roll[m]
              turn_skip = 1
              bonus = 2
            }else if(p1_index[j] <= 51 && (p1_index[j] + roll[m]) >= 52)
            {#moving to safety,which is last 5 squares
              p1_index[j] = p1_index[j] + roll[m]
              token = j
              choice = roll[m]
              turn_skip = 1
            }
          }
        }
      }
    }
  }
  
  
  if(p1_skill == 1)#movement of aggressive player if skilled movements are not possible
  {
    if(sum(p1_active == c(0,0,0,0)) < 4 && (sum(p1_index<= 57) == 4))
    {
      for(m in 1:length(roll))
      {
        if((p1_index[1]+t_roll[m]) <= 57 && turn_skip == 0)
        {
          p1_index[1] = p1_index[1] + t_roll[m]
          token = 1
          turn_skip = 1
          choice = t_roll[m]
        } else if((p1_index[2]+t_roll[m])<=57 && turn_skip ==0)
        {
          p1_index[2] = p1_index[2] + t_roll[m]
          token = 2
          turn_skip = 1
          choice = t_roll[m]
        }else if((p1_index[3]+t_roll[m])<=57 && turn_skip == 0) 
        {
          p1_index[3] = p1_index[3] + t_roll[m]
          token = 3
          turn_skip = 1
          choice = t_roll[m]
        }else if((p1_index[4]+t_roll[m])<=57 && turn_skip == 0)
        {
          p1_index[4] = p1_index[4] + t_roll[m]
          token = 4
          turn_skip = 1
          choice = t_roll[m]
        }
      }
    }
  }else if(p1_skill == 0)#movement for unskilled player
  {
    if(sum(p1_active == c(0,0,0,0)) < 4 && (sum(p1_index<= 57) == 4))
    {
      for(m in 1:length(roll))
      {
        if((p1_index[1]+roll[m]) <= 57 && turn_skip == 0)
        {
          p1_index[1] = p1_index[1] + roll[m]
          token = 1
          turn_skip = 1
          choice = roll[m]
          for(j in 1:4)#checking for captures
          {
            if(abs(p1_index[1]-p2_index[j])==26 && !(p1_index[1] %in% safe) && !(p2_index[j] %in% safe) && p2_index[j] %in% unique_elements)
            {
              p2_index[j] = 1
              bonus = 2
            }
          }
        } else if((p1_index[2]+roll[m])<=57 && turn_skip == 0)
        {
          p1_index[2] = p1_index[2] + roll[m]
          token = 2
          turn_skip = 1
          choice = roll[m]
          for(j in 1:4)
          {
            if(abs(p1_index[2]-p2_index[j])==26 && !(p1_index[2] %in% safe) && !(p2_index[j] %in% safe)  && p2_index[j] %in% unique_elements)
            {
              p2_index[j] = 1
              bonus = 2
            }
          }
        }else if((p1_index[3]+roll[m])<=57 && turn_skip == 0) 
        {
          p1_index[3] = p1_index[3] + roll[m]
          token = 3
          turn_skip = 1
          choice = roll[m]
          for(j in 1:4)
          {
            if(abs(p1_index[3]-p2_index[j])==26 && !(p1_index[3] %in% safe) && !(p2_index[j] %in% safe)  && p2_index[j] %in% unique_elements)
            {
              p2_index[j] = 1
              bonus = 2
            }
          }
        }else if((p1_index[4]+roll[m])<=57 && turn_skip == 0)
        {
          p1_index[4] = p1_index[4] + roll[m]
          token = 4
          turn_skip = 1
          choice = roll[m]
          for(j in 1:4)
          {
            if(abs(p1_index[4]-p2_index[j])==26 && !(p1_index[4] %in% safe) && !(p2_index[j] %in% safe) && p2_index[j] %in% unique_elements)
            {
              p2_index[j] = 1
              bonus = 2
            }
          }
        }
      }
    } 
  }
  
  
  #changing responsible pair to aggressive if one of opponent's tokens is close to promotion
  if(p1_skill == 4 && turn_skip == 0)
  {
    if(sum(p2_index >= 52)>0)
    {
      sub_p1_index = p1_index[which(p1_index < 57)]
      lead = which(sub_p1_index == max(sub_p1_index))[1]
      for(m in 1:length(roll))
      {
        if((p1_index[lead]+t_roll[m])<=57)
        {
          token = lead
          p1_index[token] = p1_index[token] + t_roll[m]
          turn_skip = 1
          choice = t_roll[m]
        }
      }
    }
  }
  
  #mechanism of chasing the opponent's token by responsible pair player if opponent token in a range of 6 of the last two responsible pair tokens
  if(p1_skill == 4)
  {
    for(i in 1:4)
    {
      for(j in 3:4)
      {
        for(m in 1:length(roll))
        {
          if(abs(p1_index[j] + 26 - p2_index[i]) <= 6 && turn_skip == 0 && !(p2_index[i] %in% safe) && (p1_index[j] + roll[m]) <= 27)
          {
            token = j
            p1_index[j] = p1_index[j] + t_roll[m]
            turn_skip = 1
            choice = t_roll[m]
          }
        }
      }
    }
    
    #movement of responsible pair player if no skilled movements possible
    for(m in 1:length(roll))
    {
      if(turn_skip == 0)
      {
        if((p1_index[1] + t_roll[m]) <= 27 && (move %% 2) == 1)
        {
          p1_index[1] = p1_index[1] + t_roll[m]
          turn_skip =1
          choice = t_roll[m]
          token = 1
        }else if((p1_index[2] + t_roll[m]) <= 27 && (move %% 2) == 0)
        {
          p1_index[2] = p1_index[2] + t_roll[m]
          turn_skip =1
          choice = t_roll[m]
          token = 2
        }else if((p1_index[3] + t_roll[m]) <= 27 && (move %% 2) == 1)
        {
          p1_index[3] = p1_index[3] +t_roll[m]
          turn_skip =1
          choice = t_roll[m]
          token = 3
        }else if((p1_index[4] + t_roll[m]) <= 27 && (move %% 2) == 0)
        {
          p1_index[4] = p1_index[4] +t_roll[m]
          turn_skip =1
          choice = t_roll[m]
          token = 4
        }else if((p1_index[1] + t_roll[m]) <= 27 && (move %% 2) == 0)
        {
          p1_index[1] = p1_index[1] +t_roll[m]
          turn_skip =1
          choice = t_roll[m]
          token = 1
        }else if((p1_index[2] +t_roll[m]) <= 27 && (move %% 2) == 1)
        {
          p1_index[2] = p1_index[2] + t_roll[m]
          turn_skip =1
          choice = t_roll[m]
          token = 2
        }else if((p1_index[3] +t_roll[m]) <= 27 && (move %% 2) == 0)
        {
          p1_index[3] = p1_index[3] + t_roll[m]
          turn_skip =1
          choice = t_roll[m]
          token = 3
        }else if((p1_index[4] + t_roll[m]) <= 27 && (move %% 2) == 1)
        {
          p1_index[4] = p1_index[4] +t_roll[m]
          turn_skip =1
          choice = t_roll[m]
          token = 4
        }else if((p1_index[1] + t_roll[m]) <= 57 && (move %% 2) == 1)
        {
          p1_index[1] = p1_index[1] + t_roll[m]
          turn_skip =1
          choice = t_roll[m]
          token = 1
        }else if((p1_index[2] + t_roll[m]) <= 57 && (move %% 2) == 0)
        {
          p1_index[2] = p1_index[2] + t_roll[m]
          turn_skip =1
          choice = t_roll[m]
          token = 2
        }else if((p1_index[3] + t_roll[m]) <= 57 && (move %% 2) == 1)
        {
          p1_index[3] = p1_index[3] + t_roll[m]
          turn_skip =1
          choice = t_roll[m]
          token = 3
        }else if((p1_index[4] + t_roll[m]) <= 57 && (move %% 2) == 0)
        {
          p1_index[4] = p1_index[4] + t_roll[m]
          turn_skip =1
          choice = t_roll[m]
          token = 4
        }else if((p1_index[1] + t_roll[m]) <= 57)
        {
          p1_index[1] = p1_index[1] + t_roll[m]
          turn_skip =1
          choice = t_roll[m]
          token = 1
        }else if((p1_index[2] + t_roll[m]) <= 57)
        {
          p1_index[2] = p1_index[2] + t_roll[m]
          turn_skip =1
          choice = t_roll[m]
          token = 2
        }else if((p1_index[3] + t_roll[m]) <= 57)
        {
          p1_index[3] = p1_index[3] + t_roll[m]
          turn_skip =1
          choice = t_roll[m]
          token = 3
        }else if((p1_index[4] + t_roll[m]) <= 57)
        {
          p1_index[4] = p1_index[4] + t_roll[m]
          turn_skip =1
          choice = t_roll[m]
          token = 4
        }
      }
    }
  }
  
  
  
  for(i in 1:4)#bonus is set to 1 if a token is promoted to home
  {
    if(p1_index[i] == 57 && p1_active[i] == 1)
    {
      bonus = 1
      p1_active[i] = 0
    }
  }
  if(choice == 6)
  {
    if(bonus < 3)#bonus is set to 3 if roll is the first consecutive six
    {
      bonus =3
    }else if(bonus == 3) #bonus is set to 4 if roll is the second consecutive six
    {
      bonus = 4
    }else if(bonus == 4) # bonus is set to 5 if roll is the third consecutive six
    {
      bonus = 5
    }
  }
  if(bonus %in% 3:4 && choice<6)#setting bonus back to 0 if the streak of sixes is broken
    bonus = 0
  
  if(bonus == 5)#discarding the third consecutive six and setting bonus back to 0
  {
    p1_index[token] = p1_index[token] - 6
    bonus = 0
  }
  
  
  turn_skip = 0 #resetting turn_skip
  
  return(c(p1_active,p1_index,p2_active,p2_index,choice,bonus,token))
  
}
#roll_choice(c(1,1,1,1,12,2,2,2),rep(1,8),1,4,1,dice_roll(),1,2)


turn_simulate = function(red_skill,blue_skill,initial_state,initial_red_move,initial_blue_move,initial_roll, d = 3)#function to simulate the game
{
  bmove = initial_blue_move - 1
  rmove = initial_red_move - 1
  red_store =  matrix(rep(initial_state[1:8],(1.5*n)),byrow=T,ncol=8)
  blue_store = matrix(rep(initial_state[9:16],(1.5*n)),byrow=T,ncol=8)
  red_ini = red_store
  blue_ini = blue_store
  bcap = 0
  rcap = 0
  roll_av = matrix(0,nrow=(1.5*n),ncol=6)
  roll_ch = matrix(0,nrow=(1.5*n),ncol=2)
  token_ch = roll_ch
  if(initial_blue_move %% 3 == 1 && rmove == bmove)
  {
    roll = initial_roll
  }else if(initial_blue_move %% 3 == 1 && rmove > bmove)
  {
    roll = initial_roll[1:2]
  }else if(initial_blue_move %% 3 == 2 && rmove == bmove)
  {
    roll = initial_roll[1]
  }else if(initial_blue_move %% 3 == 2 && rmove > bmove)
  {
    roll = initial_roll
  }else if(initial_blue_move %% 3 == 0 && rmove == bmove)
  {
    roll = initial_roll[1:2]
  }else
  {
    roll = initial_roll[1]
  }
  
  for(move in initial_blue_move:(1.5*n))
  {
    if(move %% 3 == 1) #considering the first move of an odd turn
    {
      if(rmove == bmove)
      {
        if(move != initial_blue_move)
        {
          roll = dice_roll(d)
        }
        if(move != 1)#considering all moves except the first one
        {
          #simulating first roll of first player,which is the first roll in an odd turn
          temp = roll_choice(red_store[(move-1),],blue_store[(move-1),],red_skill,blue_skill,1,roll,move,0)
          red_store[move,] = temp[1:8]
          blue_store[move,] = temp[9:16]
          bonus = temp[18]
          roll_ch[move,1] = temp[17]
          token_ch[move,1] = temp[19]
          roll_av[move,1:3] = c(roll,rep(0,3-length(roll)))
          for(i in 1:10)
          {
            if(bonus  %in% 1:2)
            {
              temp1 = roll_choice(red_store[move,],blue_store[move,],red_skill,blue_skill,1,dice_roll(d)[1],move,0)
              red_store[move,] = temp1[1:8]
              blue_store[move,] = temp1[9:16]
              bonus = temp1[18]
              if(bonus == 2)
              {
                rcap = rcap + 1
              }
            }else if(bonus %in% 3:4)
            {
              temp1 = roll_choice(red_store[move,],blue_store[move,],red_skill,blue_skill,1,dice_roll(d)[1],move,bonus)
              red_store[move,] = temp1[1:8]
              blue_store[move,] = temp1[9:16]
              bonus = temp1[18]
              if(bonus == 2)
              {
                rcap = rcap + 1
              }
            }
          }
        }else #considering the first move
        {
          #simulating first roll of first player,which is the first roll of an odd turn
          
          temp = roll_choice(red_store[move,],blue_store[move,],red_skill,blue_skill,1,roll,move,0)#(move)th row is used as (move-1) row does not exist here
          red_store[move,] = temp[1:8]
          blue_store[move,] = temp[9:16]
          roll_ch[move,1] = temp[17]
          token_ch[move,1] = temp[19]
          roll_av[move,1:3] = c(roll,rep(0,3-length(roll)))
          #print(c(red_store,blue_store))
          bonus = temp[18]
          for(i in 1:10)
          {
            if(bonus  %in% 1:2)
            {
              temp1 = roll_choice(red_store[move,],blue_store[move,],red_skill,blue_skill,1,dice_roll(d)[1],move,0)
              red_store[move,] = temp1[1:8]
              blue_store[move,] = temp1[9:16]
              bonus = temp1[18]
            }else if(bonus %in% 3:4)
            {
              temp1 = roll_choice(red_store[move,],blue_store[move,],red_skill,blue_skill,1,dice_roll(d)[1],move,bonus)
              red_store[move,] = temp1[1:8]
              blue_store[move,] = temp1[9:16]
              bonus = temp1[18]
            }
          }
        }
        roll = roll[-which(roll == temp[17])[1]]#updating the pool of rolls
        rmove = rmove + 1
      }
      #simulating first roll of second player,which is the second roll in an odd turn
      temp = roll_choice(red_store[move,],blue_store[move,],red_skill,blue_skill,2,roll,move,0)
      blue_store[move,] = temp[1:8]
      red_store[move,] = temp[9:16]
      bonus = temp[18]
      roll_ch[move,2] = temp[17]
      token_ch[move,2] = temp[19]
      roll_av[move,4:6] = c(roll,rep(0,3-length(roll)))
      if(bonus == 2)
      {
        bcap = bcap + 1
      }
      for(i in 1:10)
      {
        if(bonus  %in% 1:2)
        {
          temp1 = roll_choice(red_store[move,],blue_store[move,],red_skill,blue_skill,2,dice_roll(d)[1],move,0)
          blue_store[move,] = temp1[1:8]
          red_store[move,] = temp1[9:16]
          bonus = temp1[18]
          if(bonus == 2)
          {
            bcap = bcap + 1
          }
        }else if(bonus %in% 3:4)
        {
          temp1 = roll_choice(red_store[move,],blue_store[move,],red_skill,blue_skill,2,dice_roll(d)[1],move,bonus)
          blue_store[move,] = temp1[1:8]
          red_store[move,] = temp1[9:16]
          bonus = temp1[18]
          if(bonus == 2)
          {
            bcap = bcap + 1
          }
        }
      }
      roll = roll[-which(roll == temp[17])[1]]
      bmove = bmove + 1
    }else if(move %% 3 == 2)
    {
      #simulating second roll of first player,which is the third roll in an odd turn
      if(rmove == bmove)
      {
        temp = roll_choice(red_store[(move-1),],blue_store[(move-1),],red_skill,blue_skill,1,roll,move,0)
        red_store[move,] = temp[1:8]
        blue_store[move,] = temp[9:16]
        bonus = temp[18]
        roll_ch[move,1] = temp[17]
        token_ch[move,1] = temp[19]
        roll_av[move,1:3] = c(roll,rep(0,3-length(roll)))
        if(bonus == 2)
        {
          rcap = rcap + 1
        }
        for(i in 1:10)
        {
          if(bonus  %in% 1:2)
          {
            temp1 = roll_choice(red_store[move,],blue_store[move,],red_skill,blue_skill,1,dice_roll(d)[1],move,0)
            red_store[move,] = temp1[1:8]
            blue_store[move,] = temp1[9:16]
            bonus = temp1[18]
            if(bonus == 2)
            {
              rcap = rcap + 1
            }
          }else if(bonus %in% 3:4)
          {
            temp1 = roll_choice(red_store[move,],blue_store[move,],red_skill,blue_skill,1,dice_roll(d)[1],move,bonus)
            red_store[move,] = temp1[1:8]
            blue_store[move,] = temp1[9:16]
            bonus = temp1[18]
            if(bonus == 2)
            {
              rcap = rcap + 1
            }
          }
        }
        rmove = rmove + 1
      }
      
      if(move != initial_blue_move || initial_red_move == initial_blue_move)
      {
        roll = dice_roll(d)#rolling dice in beginning of a turn
      }
      #simulating the first roll of the second player,which is the first roll in an even turn
      temp = roll_choice(red_store[move,],blue_store[move,],red_skill,blue_skill,2,roll,move,0)
      blue_store[move,] = temp[1:8]
      red_store[move,] = temp[9:16]
      bonus = temp[18]
      roll_ch[move,2] = temp[17]
      token_ch[move,2] = temp[19]
      roll_av[move,4:6] = c(roll,rep(0,3-length(roll)))
      if(bonus == 2)
      {
        bcap = bcap + 1#updating number of captures
      }
      for(i in 1:10)#considering extra turns, 10 is considered to be an upper bound of number of possible consecutive extra turns
      {
        if(bonus %in% 1:2)#if extra turn is due to a promotion or capture 
        {
          temp1 = roll_choice(red_store[move,],blue_store[move,],red_skill,blue_skill,2,dice_roll(d)[1],move,0)
          blue_store[move,] = temp1[1:8]
          red_store[move,] = temp1[9:16]
          bonus = temp1[18]
          if(bonus == 2)
          {
            bcap = bcap + 1
          }
        }else if(bonus %in% 3:4)#if extra turn is due to a roll of six
        {
          temp1 = roll_choice(red_store[move,],blue_store[move,],red_skill,blue_skill,2,dice_roll(d)[1],move,bonus)
          blue_store[move,] = temp1[1:8]
          red_store[move,] = temp1[9:16]
          bonus = temp1[18]
          if(bonus == 2)
          {
            bcap = bcap + 1
          }
        }
      }
      
      roll = roll[-which(roll == temp[17])[1]]
      #removing the chosen roll from the pool of rolls
      bmove = bmove + 1
    }else if(move %% 3 == 0)
    {
      #simulating first roll of first player,which is the second roll in an even turn
      if(rmove == bmove)
      {
        temp = roll_choice(red_store[(move-1),],blue_store[(move-1),],red_skill,blue_skill,1,roll,move,0)#(move-1)th row of the storing matrices are considered as for beginning of each move,the indices of the tokens at the end of the last move is considered
        red_store[move,] = temp[1:8]
        blue_store[move,] = temp[9:16]
        bonus = temp[18]
        roll_ch[move,1] = temp[17]
        token_ch[move,1] = temp[19]
        roll_av[move,1:3] = c(roll,rep(0,3-length(roll)))
        if(bonus == 2)
        {
          rcap = rcap + 1
        }
        for(i in 1:10)#considering extra turns
        {
          if(bonus %in% 1:2)
          {
            temp1 = roll_choice(red_store[move,],blue_store[move,],red_skill,blue_skill,1,dice_roll(d)[1],move,0)
            red_store[move,] = temp1[1:8]
            blue_store[move,] = temp1[9:16]
            bonus = temp1[18]
            if(bonus == 2)
            {
              rcap = rcap + 1
            }
          }else if(bonus %in% 3:4)
          {
            temp1 = roll_choice(red_store[move,],blue_store[move,],red_skill,blue_skill,1,dice_roll(d)[1],move,bonus)
            red_store[move,] = temp1[1:8]
            blue_store[move,] = temp1[9:16]
            bonus = temp1[18]
            if(bonus == 2)
            {
              rcap = rcap + 1
            }
          }
        }
        roll = roll[-which(roll == temp[17])[1]]
        rmove = rmove + 1
      }
      #simulating second roll of second player,which is the third roll in an even turn
      temp = roll_choice(red_store[move,],blue_store[move,],red_skill,blue_skill,2,roll,move,0)
      blue_store[move,] = temp[1:8]
      red_store[move,] = temp[9:16]
      bonus = temp[18]
      roll_ch[move,2] = temp[17]
      token_ch[move,2] = temp[19]
      roll_av[move,4:6] = c(roll,rep(0,3-length(roll)))
      if(bonus == 2)
      {
        bcap = bcap + 1
      }
      for(i in 1:10)
      {
        if(bonus  %in% 1:2)
        {
          temp1 = roll_choice(red_store[move,],blue_store[move,],red_skill,blue_skill,2,dice_roll(d)[1],move,0)
          blue_store[move,] = temp1[1:8]
          red_store[move,] = temp1[9:16]
          bonus = temp1[18]
          if(bonus == 2)
          {
            bcap = bcap + 1
          }
        }else if(bonus %in% 3:4)
        {
          temp1 = roll_choice(red_store[move,],blue_store[move,],red_skill,blue_skill,2,dice_roll(d)[1],move,bonus)
          blue_store[move,] = temp1[1:8]
          red_store[move,] = temp1[9:16]
          bonus = temp1[18]
          if(bonus == 2)
          {
            bcap = bcap + 1
          }
        }
      }
      bmove = bmove + 1
    }
    
  }
  
  return(cbind(1:(1.5*n),red_store[,5:8],blue_store[,5:8],roll_av,roll_ch,token_ch))#returning the game state after each move,and the number of captures made by either player
  
}

#turn_simulate(1,4,rep(1,16),1,1,dice_roll(1))[,2:9]

#for implementing parallel processing
cores = detectCores()
c1 <- makeCluster(cores[1] - 1)
registerDoParallel(c1)
n_sim = 1000
finalMatrix <- foreach(i = 1:n_sim, .combine = rbind) %dopar% {
  tempMatrix = turn_simulate(4,4,rep(1,16),1,1,dice_roll(3)) #for particular strategy pair, can be configured. (4,4) indicates the strategy tuple
  red_ini = rbind(c(1,1,1,1),tempMatrix[1:(1.5*n - 1),2:5])
  blue_ini = rbind(c(1,1,1,1),tempMatrix[1:(1.5*n - 1),6:9])
  red_score = numeric(length = (1.5*n))
  blue_score = red_score
  for(i in 1:(1.5*n))
  {
    red_score[i] = (sum(tempMatrix[i,2:5] == 57)*56) + sum(tempMatrix[i,2:5] - 1)
    blue_score[i] = (sum(tempMatrix[i,6:9] == 57)*56) + sum(tempMatrix[i,6:9] - 1)
  }
  tempMatrix = cbind(tempMatrix[,1:9],red_ini,blue_ini,tempMatrix[,10:19],red_score,blue_score)
  tempMatrix = rbind(tempMatrix,rep(0))
  tempMatrix
}
stopCluster(c1)

red_pt = finalMatrix[seq(1.5*n,nrow(finalMatrix),1.5*n + 1),28] #end points scored by first player
blue_pt = finalMatrix[seq(1.5*n,nrow(finalMatrix),1.5*n + 1),29] #end points scored by second player
#sum(red_pt > blue_pt) + 0.5*sum(red_pt == blue_pt)

#colnames(finalMatrix) = c("Move",paste0("Player_1_Final",1:4),paste0("Player_2_Final",1:4),paste0("Player_1_Initial",1:4),paste0("Player_2_Initial",1:4),rep("Player 1 Rolls available",3),rep("Player 2 Rolls available",3),"Player 1 Roll Chosen","Player 2 Roll Chosen","Player 1 Token Chosen","Player 2 Token Chosen","Player1 Score","Player 2 Score")
#write.csv(finalMatrix,file="Aggressive_vs_Responsible.csv")


#tempMatrix = turn_simulate(1,4,rep(1,16),1,1,dice_roll())[,c(1,6:9,14:17)]


point_system = function(red_skill,blue_skill,initial_state,initial_red_move,initial_blue_move,initial_roll)#function to compute points scored from final game state
{
  store = turn_simulate(red_skill,blue_skill,initial_state,initial_red_move,initial_blue_move,initial_roll)
  n1 = 1.5*n
  store[n1,]
  #computing number of promoted tokens
  ractive_fin = sum(store[n1,2:5] == 57)
  bactive_fin = sum(store[n1,6:9] == 57)
  #computing total of final indices for each player
  rindex_fin = store[n1,2:5] - 1
  bindex_fin = store[n1,6:9] - 1
  #computing final points for each player
  red_pt = sum(rindex_fin) + (ractive_fin * 56)
  blue_pt = sum(bindex_fin) + (bactive_fin * 56)
  return(c(red_pt,blue_pt,store[n1,17:18]))
}


skill = matrix(rep(c(0,1,4),3),nrow=3,byrow=T)#matrix consisting of different player skills

simulate_game = function(red_skill,blue_skill,initial_state,initial_red_move,initial_blue_move,initial_roll,fixed_roll,n_sim)#function to simulate the game a required number of times
{
  #simulating the game n_sim times
  bcount = 0
  rcount = 0
  tie = 0
  cap_count = matrix(0,nrow=n_sim,ncol=2)
  pt = matrix(0,nrow=n_sim,ncol=2)
  store_temp = numeric(n_sim)
  for(m in 1:n_sim)
  {
    if(fixed_roll == 0)
    {
      initial_roll = dice_roll()
    }
    store_sim = point_system(red_skill,blue_skill,initial_state,initial_red_move,initial_blue_move,initial_roll)
    
    #computing number of wins for each player
    if(store_sim[1] > store_sim[2])
    {
      store_temp[m] = 1
    }else if(store_sim[1] < store_sim[2])
    {
      store_temp[m] = 0
    }else
    {
      store_temp[m] = 0.5
    }
    pt[m,] = store_sim[1:2]
    cap_count[m,] = store_sim[3:4]
    if(red_skill == 3 || blue_skill == 3)
      print(m)
  }
  rcount = sum(store_temp)
  bcount = n_sim - rcount
  return(c(store_temp,rcount,bcount,c(summary(pt)[c(1,3,4,6),],c(summary(cap_count)[c(1,3,4,6),],sd(pt[,1]),sd(pt[,2]),sd(cap_count[,1]),sd(cap_count[,2])))))
}



#sum(as.numeric(finalMatrix[,p+1]))
#sum(as.numeric(finalMatrix[,p+2]))
#new = matrix(0,nrow=50,ncol=p+22)
#new[,1:(p+2)] = as.numeric(finalMatrix[,1:(p+2)])
#new[,(p+19):(p+22)] = as.numeric(finalMatrix[,(p+19):(p+22)])
#for(i in 3:18)
#{
# new[,(p+i)] = as.numeric(unlist(strsplit(finalMatrix[,(p+i)] ,":"))[seq(2,100,2)])
#}

#write.csv(new,"results_aggvsmts.csv")


#the following lines of code can be used to display summary statistics like mean and sd of points scored by each player and win percentage of first player
#to  be displayed if necessary
#split_string1 <- unlist(strsplit(output[,7] ,":"))[seq(2,(2*nrow(skill)*ncol(skill)),2)]
#split_string2 <- unlist(strsplit(output[,11],":"))[seq(2,(2*nrow(skill)*ncol(skill)),2)]
#as.numeric(split_string1)
#as.numeric(split_string2)
#as.numeric(output[,3])/(n_sim/100)
#output[,21:22]



