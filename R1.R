best<- function(state, oc){
  outcome_short<- outcome[c("Hospital.Name", "State","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
  #outcome_short <- outcome_short[complete.cases(outcome_short),]
  outcome_short <- transform(outcome_short, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure = as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),
            Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia = as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
  state_subset <- subset(outcome_short, State == state)

  if(oc== "heart attack"){
    state_subset <- state_subset[!is.na(state_subset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
    m<-min(state_subset[3])
    fin<- subset(state_subset, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack<=m)
  }
  else if(oc == "heart failure"){
    state_subset <- state_subset[!is.na(state_subset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
    m<-min(state_subset[4])
    fin<- subset(state_subset, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure<=m)
  }
  else if(oc == "pneumonia"){
    state_subset <- state_subset[!is.na(state_subset$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
    m<-min(state_subset[5])
    fin<- subset(state_subset, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia<=m)
  }
  else{
    print("wrong choice")
  }
  
  print(fin[["Hospital.Name"]])
  
}