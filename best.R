library("dplyr")

best=function(state,outcome){
        outcome_of_care_measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## outcome_use=data.frame(outcome[,2],outcome[,7],outcome[,11],outcome[,17],outcome[,23])
        stateRange=outcome_of_care_measures$State
        outcomeRange=c("heart attack","heart failure","pneumonia")

        outcome_of_care_measures$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack=as.numeric(outcome_of_care_measures$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
        outcome_of_care_measures$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure=as.numeric(outcome_of_care_measures$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
        outcome_of_care_measures$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia=as.numeric(outcome_of_care_measures$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
        
        
        
        if (outcome=="heart attack"){index=11}
        else if (outcome=="heart failure"){index=17}
        else if (outcome=="pneumonia"){index=23}

        
        selectedState=filter(outcome_of_care_measures,outcome_of_care_measures$State==state)
        
        validOutcomeIndex=complete.cases(selectedState[,index])
        validOutcome=selectedState[validOutcomeIndex,]
        
        sorted=arrange(validOutcome,desc(validOutcome[,index]))
        numberOfRows=nrow(sorted)
        sorted[numberOfRows,2]

## error handling        

}