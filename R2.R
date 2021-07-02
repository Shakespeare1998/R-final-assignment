rankhospital<- function(state, oc, rnk){
  outcome_short<- outcome[c("Hospital.Name", "State","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
  #outcome_short <- outcome_short[complete.cases(outcome_short),]
  outcome_short <- transform(outcome_short, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure = as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),
                             Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia = as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
  state_subset <- subset(outcome_short, State == state)

  if(oc== "heart attack"){
    state_subset <- state_subset[!is.na(state_subset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
    fin <- state_subset[order(state_subset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, state_subset$Hospital.Name),]
    }
  else if(oc == "heart failure"){
    state_subset <- state_subset[!is.na(state_subset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
    fin<- state_subset[order(state_subset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, state_subset$Hospital.Name),]
  }
  else if(oc == "pneumonia"){
    state_subset <- state_subset[!is.na(state_subset$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
    fin<- state_subset[order(state_subset$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, state_subset$Hospital.Name),]
    #print(head(fin,11))
    }
  else{
    print("wrong choice")
  }
  if (rnk == "best"){
    rnk=1
  }
  if (rnk == "worst"){
    rnk = nrow(fin)
  }
  
  print(fin[rnk, "Hospital.Name"])
  
}