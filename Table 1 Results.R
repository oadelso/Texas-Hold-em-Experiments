#Console input for result on approximation of the 5-card poker hand's probabilities 
#from seven, randomly selected cards from a 52 card deck.

#This simulations uses a derivative of the R function 'handmaker' named 'handmakertwo'
#'handmakertwo' goes through all possible combinations of 7 cards, and returns the highest
#5-card hand possible. This is an integer between 1 and 10, inclusive.

MC1=function(number){
  #MC1=First Monte Carlo Simulation
  MC1=rep(0,number);#vector where results will be stored
  for (i in 1:number){
    #will run the cards and hand functions a specified number
    #of times, and will store the resulting type of hand in 'y'
    y=cards(7);
    MC1[i]=handmakertwo(y[,1:2],y[,3:7],3);
  }
  #the resulting vector can then be analyzed to view how many
  #of the total runs were a card high (i.e.,MC1[i]=1), a pair (i.e., MC[i]=2), etc.
  return(MC1);
}

Results=matrix(0,1,10)
#for reproducibility set seed to a specified number
set.seed(10)
Output=MC1(1000000) #total of 1.0 MM simultions
for (i in 1:10){
  #see the frequency of the 10 poker hands
  Results[i]=sum(Output==i)/1E6;
}

#print Results
Results
