rankall<- function(oc, rnk){
  outcome_short<- outcome[c("Hospital.Name", "State","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
  outcome_short <- transform(outcome_short, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure = as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),
                             Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia = as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
  outcome_short[,2] <- as.character(outcome_short[,2])
  state_list <- unique(outcome_short[c("State")])
  final_li <- vector()
  if (rnk == "best"){
    rnk=1
  }
  if (rnk == "worst"){
    rnk = nrow(fin)
  }

  #final_list <- outcome_short["Hospital", "State"]
  
  for(i in 1:length(state_list)){
    state_subset <- subset(outcome_short, State == state[i])
    
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
  }
  else{
    print("wrong choice")
  }
  hosp <- fin[rnk, "Hospital.Name"]
  
  final_li <- append(final_li, c(hosp, state_list[i]))
  }
print(final_li)
}