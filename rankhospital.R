rankhospital <- function(state, outcome, num = "best") {
        
        ## Read outcome data
        outcome_of_care_measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## [2] "Hospital.Name"
        ## [7] "State"
        ## [11] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" 
        ## [17] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        ## [23] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" 
        
        ## convert character into numeric
        outcome_of_care_measures[,11]=as.numeric(outcome_of_care_measures[,11])
        outcome_of_care_measures[,17]=as.numeric(outcome_of_care_measures[,17])
        outcome_of_care_measures[,23]=as.numeric(outcome_of_care_measures[,23])
        
        
        
        
        ## Check that state and outcome are valid
        validState=names(table(outcome_of_care_measures$State))
        validOutcome=c("heart attack","heart failure","pneumonia")
        
        if (length(grep(state,validState))==0){
                stop("invalid state")
        }
        else if (length(grep(outcome,validOutcome))==0){
                stop("invalid outcome")
        }
        
        
        
        
     
        ## Hospitals that do not have data on a particular outcome should
        ## be excluded from the set of hospitals when deciding the rankings
        if (outcome=="heart attack"){index=11}
        else if (outcome=="heart failure"){index=17}
        else if (outcome=="pneumonia"){index=23}
        
        dfSelectedState=filter(outcome_of_care_measures,outcome_of_care_measures$State==state)
        removeNaOutcomeIndex=complete.cases(dfSelectedState[,index])
        dfRemoveNaOutcome=dfSelectedState[removeNaOutcomeIndex,]
        
       
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        outcomeOrderIndex=order(dfRemoveNaOutcome[,index])
        
        if (num=="best"){rankIndex=outcomeOrderIndex[1]}
        else if (num=="worst"){rankIndex=outcomeOrderIndex[length(outcomeOrderIndex)]}
        else {
                ## If the number given by num is larger than the number of hospitals 
                ## in that state, then the function should return NA
                if (num>nrow(dfRemoveNaOutcome)){
                        stop("NA")
                }
                rankIndex=outcomeOrderIndex[num]
                }
       
        dfRemoveNaOutcome[rankIndex,c(2,index)]

        
}