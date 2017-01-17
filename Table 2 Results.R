#Console input for the results shown in table 2, comparing approximated values for the probability of winning
#a showdown with one opponent with AAs as hole cards, with varying number of simulations, with the actual value
#of roughly 84.931915%.

#for reproducibility set seed
set.seed(10)
#create vector 'y' and dummy variable 'yt'
yt=0;
y=matrix(0,1,6);
#Define the user's hole cards
usercards=matrix(c(1,1,1,2), nrow=2,ncol=2);

for (i in 1:6){
  #save probability in variable 'y'
  #run 'OddsNL_simple' R function to approximate the probability of winning
  #having the aforementioned hole cards, by showndown (i.e., the river bettering
  #round, or '3'), carrying out 10^i number of simultions
  yt=oddsNL_simple(usercards,3,1,10^i);
  #'OddsNL' has a 1 by 2 matrix output, we need only the winning probability, which is the
  #first value of the matrix
  y[i]=yt[1];
}

#print out results
y
