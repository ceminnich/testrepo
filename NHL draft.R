pickOdds <- data.frame(team=c(1:15),pick1=c(.185,.135,.115,.095,.085,.075,.065,.06,.05,.035,.03,.025,.02,.015,.01))
draftOrder <- data.frame(pick1=0,pick2=0,pick3=0,prob1=0,prob2=0,prob3=0)
draftOrder <- draftOrder[0,]

for (pick1 in 1:15) {
  for (pick2 in 1:15) {
    for (pick3 in 1:15) {
      if (pick1 != pick2 & pick1 !=pick3 & pick2!=pick3) {
        draftOrder <- rbind(draftOrder, data.frame(pick1, pick2, pick3))
      }
    }
  }
}

draftOrder$prob1 <- 0
draftOrder$prob2 <- 0
draftOrder$prob3 <- 0

for (i in 1:nrow(draftOrder)) {
  draftOrder[i,]$prob1 <- pickOdds[pickOdds$team==draftOrder[i,'pick1'],'pick1']
  draftOrder[i,]$prob2 <- pickOdds[pickOdds$team==draftOrder[i,'pick2'],'pick1']
  draftOrder[i,]$prob3 <- pickOdds[pickOdds$team==draftOrder[i,'pick3'],'pick1']
}

draftOrder$prob3 <- draftOrder$prob3/(1-draftOrder$prob1-draftOrder$prob2)
draftOrder$prob2 <- draftOrder$prob2/(1-draftOrder$prob1)
draftOrder$totProb <- draftOrder$prob1*draftOrder$prob2*draftOrder$prob3

pickOdds$pick2 <- 0
pickOdds$pick3 <- 0

for (i in 1:nrow(pickOdds)) {
  pickOdds[i,]$pick2 <- sum(draftOrder[draftOrder$pick2 == pickOdds[i,]$team,'totProb'])
  pickOdds[i,]$pick3 <- sum(draftOrder[draftOrder$pick3 == pickOdds[i,]$team,'totProb'])
}

pickOdds$top3 <- pickOdds$pick1+pickOdds$pick2+pickOdds$pick3
