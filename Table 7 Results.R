#Console Input
#Expeiment 3
#Both agents use 10 simulations to calculate the probability of winning.
#One agent uses Fully Kelly, and the other twice the Kelly amount.
#HUTable and HUbettinground R functions are edited accordingly. 

#Edit HUTable
HUtable<-function(games=10,bigblind=1,initialw=100){
  #Input
  #'games' is an intiger value defining the amount of games or matches which will be simulated
  #'bigblind' is an integer value defining the big blind amount for the game
  #'initialw' is the initial wealth for each agent in the Heads Up match
  #Output
  #A 'games' by 12 matrix
  #for each game, the matrix saves:
  #Two values, each being the ending wealth for each agent
  #The hole cards for each agent in each match of the simulations
  #the community cards (if applicable)
  #the round in which the match ended. For example, it would be 1 if one of the agents fold pre-flop, or
  #4 if the round ends in a showdown.
  
  wealth=rep(0,2); #stores the weath of each player
  wealth[1:2]=initialw; #initial wealth for players
  HUtable=matrix(0,games,12); #will save the resulting wealth at the end of each game (2 values), all cards
  #(4+5=9 total), plus betting round in which game ended (1), total of 12 values.
  
  for (i in 1:games){
    pot=0;
    raise=0;
    play=rep(0,2); #play vector saves the play done by each of the players during the round
    moneyingame=rep(0,2); #saves the amount of money placed by each player in the pot per game
    
    #big and small blind alternate between each round
    sbindex=(i+1)%%2+1; #small blind index
    bbindex=i%%2+1;     #big blind index
    
    wealth[sbindex]=wealth[sbindex]-(bigblind)/2;
    moneyingame[sbindex]=bigblind/2;
    wealth[bbindex]=wealth[bbindex]-bigblind;
    moneyingame[bbindex]=bigblind;
    
    if (wealth[1]<0 | wealth[2]<0 ){
      #end game once any of the players goes broke
      #the money is returned to see the final status of each player's stack (AKA wealth)
      wealth[sbindex]=wealth[sbindex]+(bigblind)/2; 
      wealth[bbindex]=wealth[bbindex]+bigblind;
      communitycardsn=matrixtonumber(communitycards);
      playercardsn=matrixtonumber(playercards);
      gameoutput=c(wealth,playercardsn,communitycardsn,betround); #info to match the output of HUbettinground
      HUtable[games,]=gameoutput; #to view the last round of the match
      break
    }
    pot=bigblind*(3/2);
    gamecards=cards(9); #four cards for the players (two per player), and the five community cards
    playercards=gamecards[,1:4];
    communitycards=gamecards[,5:9];
    #round 0, small blind is first to act
    betround=0; #pre flop bet round
    sbs=(sbindex-1)*2+1;
    sbe=(sbindex-1)*2+2;
    sbcards=playercards[,sbs:sbe]; #smallblind cards
    #small blind needs to decide whether to call, raise, or fold 
    
    behavior=2;
    kellyfrac=2;
    nsimulations=10; #less informed agent
    if (sbindex==1){
      #decide whether to change parameters for the kelly bot for each agent in the game
      behavior=1;
      kellyfrac=0.5;
      nsimulations=10; #better informed agent;
    }
    
    decision=kellybot(initialw,wealth[sbindex],sbcards,0,behavior,kellyfrac,nsimulations,pot,raise,bigblind,bigblind/2);
    if (decision[1]==1){
      #player on the small blind folds
      play[sbindex]=decision[1];
      wealth[bbindex]=wealth[bbindex]+pot;
      communitycardsn=matrixtonumber(communitycards);
      playercardsn=matrixtonumber(playercards);
      gameoutput=c(wealth,playercardsn,communitycardsn,betround); #info to match the output of HUbettinground
    }else if (decision[1]==2){
      #player in the small blind calls
      wealth[sbindex]=wealth[sbindex]-bigblind/2;
      moneyingame[sbindex]=moneyingame[sbindex]+bigblind/2;
      pot=pot+bigblind/2; #increase the pot accordingly
      play[sbindex]=decision[1];
      #the other betting rounds are modeled in another function
      gameoutput=HUbettinground(bbindex,bbindex,play,raise,betround,wealth,pot,bigblind,playercards,communitycards,moneyingame,initialw);
      wealth=gameoutput[1:2]; #part of output from HUbettinground
      if (wealth[1]<0 | wealth[2]<0 ){
        #end game once any of the players goes broke
        communitycardsn=matrixtonumber(communitycards);
        playercardsn=matrixtonumber(playercards);
        gameoutput=c(wealth,playercardsn,communitycardsn,betround); #info to match the output of HUbettinground
        HUtable[games,]=gameoutput; #to view the last round of the match
        break
      }
      
    }else if (decision[1]==3){
      #small blind raises
      play[sbindex]=decision[1];
      raise=decision[2]; #the amount being raised by the small blind
      wealth[sbindex]=wealth[sbindex]-raise+moneyingame[sbindex];
      pot=pot+raise-moneyingame[sbindex]; #only the additional money goes into the pot
      moneyingame[sbindex]=raise;
      #the other betting rounds are modeled in another function
      gameoutput=HUbettinground(bbindex,bbindex,play,raise,betround,wealth,pot,bigblind,playercards,communitycards,moneyingame,initialw);
      wealth=gameoutput[1:2];
      if (wealth[1]<0 | wealth[2]<0 ){
        #end game once any of the players goes broke
        communitycardsn=matrixtonumber(communitycards);
        playercardsn=matrixtonumber(playercards);
        gameoutput=c(wealth,playercardsn,communitycardsn,betround); #info to match the output of HUbettinground
        HUtable[games,]=gameoutput; #to view the last round of the match
        break
      }
    }
    
    if (wealth[1]<0 | wealth[2]<0 ){
      #end game once any of the players goes broke
      communitycardsn=matrixtonumber(communitycards);
      playercardsn=matrixtonumber(playercards);
      gameoutput=c(wealth,playercardsn,communitycardsn,betround); #info to match the output of HUbettinground
      HUtable[games,]=gameoutput; #to view the last round of the match
      break
    }  
    HUtable[i,]=gameoutput; #saves output of the ith game
  }
  HUtable
}

#Edit HUbettinground
HUbettinground<-function(bp,bbindex,play,raise,betround,wealth,pot,bigblind,playercards,communitycards,moneyinround,initialw){
  #Input
  #'bp' is the betting position, it will be either 1 or 2; it is the position of the agent that needs to act
  #'bbindex' is the agent with the big blind player position, either 1 or 2. After each round starts anew, bp should equal bbindex
  #'play' is 2 element vector containing the play of each player during the existing round (1=fold, 2=call/check, 3=raise)
  #'raise' is any existing raise in current betting round; it should be either zero or a multiple of the big blind amount
  #'betround' is either 1=flop, 2=turn, 3=river,4=showdown
  #'wealth' is the 2 element vector containing each player's current wealth
  #'pot' is the current size of the pot
  #'big blind' is the big blind value of the game
  #'playercards' and 'communitycards' are the matrices containing the game's cards in the format explained in the 'cards' R function 
  #'moneyintheround' is a 2 element vector containg the amount of money in the pot for a specific round for each player
  #'initialw' is the amount of money that each agent began with in the beginning of the simulation
  #Output
  #The wealth of each player
  #The hole cards of each player in integer format (i.e., an integer between 1 and 52, inclusive)
  #The community cards in the round in integer format
  #Betround currently being played in each match
  
  if (isTRUE(all.equal(play,c(2,2)))){
    #both players have called and the next round of betting starts, or previous player raised and current player called
    betround=betround+1;
    if (betround==4){
      #betting round is past the river, and each player must show its cards
      handrank1=handmaker(playercards[,1:2],communitycards,3); #handrank of first player
      handrank2=handmaker(playercards[,3:4],communitycards,3); #handrank of second player
      if (handrank1>handrank2){
        #player one wins the hand
        wealth[1]=wealth[1]+pot;
      }else if (handrank2>handrank1){
        wealth[2]=wealth[2]+pot;
      }else{
        #split the pot
        wealth[1]=wealth[1]+pot/2;
        wealth[2]=wealth[2]+pot/2;
      }
      #prepare output
      communitycardsn=matrixtonumber(communitycards); #stores the community cards in number format rather than matrix
      playercardsn=matrixtonumber(playercards); #stores the player cards in number format
      HUbettinground=c(wealth,playercardsn,communitycardsn,betround); #output
    }
    else{
      #Next round starts, and player and moneyinround vectors, along with the raise variable need to be reset to 0
      play=rep(0,2);
      moneyinround=rep(0,2);
      raise=0;
      bp=bbindex; #the big blind is always the first to act on all other rounds following the flop
      HUbettinground=HUbettinground(bp,bbindex,play,raise,betround,wealth,pot,bigblind,playercards,communitycards,moneyinround,initialw);
    }
    
  } else if (play[1]==1){
    #one of the players has folded
    wealth[2]=wealth[2]+pot; #player 2 wins the pot
    #prepare output
    communitycardsn=matrixtonumber(communitycards); #stores the community cards in number format rather than matrix
    playercardsn=matrixtonumber(playercards); #stores the player cards in number format
    HUbettinground=c(wealth,playercardsn,communitycardsn,betround); #output
  } else if (play[2]==1){
    wealth[1]=wealth[1]+pot; #player 1 wins the pot
    #prepare output
    communitycardsn=matrixtonumber(communitycards); #stores the community cards in number format rather than matrix
    playercardsn=matrixtonumber(playercards); #stores the player cards in number format
    HUbettinground=c(wealth,playercardsn,communitycardsn,betround); #output
  } else{
    #next move needs to be made by the bp player
    hs=(bp-1)*2+1; #hand start index for the betting position, bp. bp is either 1 or 2
    he=(bp-1)*2+2; #hand end index for the betting position, bp. bp is either 1 or 2.
    tablecards=0;
    if (betround>0){
      ncc=betround+2; #number of community cars already known by the round; 3 for the flop, 4 for the turn, and 5 by the river.
      tablecards=communitycards[,1:ncc]; #community cards seen during the round 
    }
    
    behavior=2;
    kellyfrac=2;
    nsimulations=10; #less informed agent
    if (bp==1){
      #decide whether to change parameters for the kelly bot for each agent in the game
      behavior=1;
      kellyfrac=0.5;
      nsimulations=10; #better informed agent;
    }
    
    decision=kellybot(initialw,wealth[bp],playercards[,hs:he],tablecards,behavior,kellyfrac,nsimulations,pot,raise,bigblind,moneyinround[bp]); #decision to be made by the kelly criterion
    
    play[bp]=decision[1]; 
    if (play[bp]==3 | play[bp]==2){
      #player raises/calls
      if(play[bp]==2){
        #if player calls set play vector to c(2,2)
        play[bp%%2+1]=2;
      }
      raise=decision[2];
      wealth[bp]=wealth[bp]-raise+moneyinround[bp];
      pot=pot+raise-moneyinround[bp];
      moneyinround[bp]=raise; #NEEDS to be checked
    }
    #the other player's turn to fold/call/check or raise/re-raise
    bp=bp%%2+1;
    HUbettinground=HUbettinground(bp,bbindex,play,raise,betround,wealth,pot,bigblind,playercards,communitycards,moneyinround,initialw);
  }
  HUbettinground
}

#Carry Out the experiment with one agent running only 5 simulations to arrive at the probability of winning, while
#the other runs 10. This should lead to the better informed agent have an advantage over the less informed.

#reproducibility
set.seed(1)
Outcome=matrix(0,1,2); #store the number of times each agent wins a match
SaveWealth=matrix(0,100,2); #store wealth by the end of each match in order to get the average and standard deviation
for (i in 1:100){#100 games to play
  yt=HUtable(50,1,100); #each agent has an initial wealth of 100, with each play having 100 matches
  SaveWealth[i,1]=yt[50,1];
  SaveWealth[i,2]=yt[50,2];
  if (SaveWealth[i,1]>SaveWealth[i,2]){
    Outcome[1]=Outcome[1]+1 #agent one won
  }else if (wealth2>wealth1){
    Outcome[2]=Outcome[2]+1 #agent two won
  }else{
    #in the event the match is a tie (i.e., final wealth by end of each game), each receives half a point
    Outcome[1]=Outcome[1]+0.5
    Outcome[2]=Outcome[2]+0.5
  }
  
}

#printout results
Outcome
mean(SaveWealth[,1])
mean(SaveWealth[,2])
sd(SaveWealth[,1])
t.test(SaveWealth[,1],SaveWealth[,2])