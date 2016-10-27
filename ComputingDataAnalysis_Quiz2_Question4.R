rankhospital<- function(state,outcome, num="best"){
  
  ##read outcome data
  outcome1=read.csv('outcome-of-care-measures.csv', colClasses="character")
  
  State=as.numeric(outcome1[, "State"])
  Hospital.Name=as.numeric(outcome1[, "Hospital.Name"])
  Heart.Attack=as.numeric(outcome1[, 11])
  Heart.Failure=as.numeric(outcome1[, 17])
  Pneumonia=as.numeric(outcome1[, 23])

  my.data=data.frame(I(State),I(Hospital.Name),I(Heart.Attack), I(Heart.Failure),I(Pneumonia))
  
  ##check that state and outcome data are valid
  
  
  ###outcome= c(heart.attack, heart.failure,pneumonia)

  
  if(nrow(state.data) == 0){
    stop("Invalid state")
  }
    for (i in num){
      
       if(outcome == "heart attack"){
           order.heartattack=order(state.data$Heart.Attack,decreasing=TRUE)
           state.data$Rank=NA
           state.data$Rank[order.heartattack]=1:nrow(state.data)
      
                if(num == "best"){
                  rank=1
                  RankHospital=state.data[which(rank==1),"Hospital.Name"]
                } else if (num == "worst"){
                  rank= max(state.data$Rank)
                  RankHospital=state.data[which(state.data$Rank==rank),"Hospital.Name"]
                }else {
                  num=as.integer(num)
                  RankHospital=state.data[which(state.data$Rank==num),"Hospital.Name"]
                }
           
       }else if (outcome == "heart failure"){
               order.heartfailure=order(state.data$Heart.Failure)
               state.data$Rank=NA
               state.data$Rank[order.heartfailure]=1:nrow(state.data)
               
               if(num == "best"){
                 rank=1
                 RankHospital=state.data[which(rank==1),"Hospital.Name"]
               } else if (num == "worst"){
                 rank= max(state.data$Rank)
                 RankHospital=state.data[which(state.data$Rank==rank),"Hospital.Name"]
               }else {
                 num=as.integer(num)
                 RankHospital=state.data[which(state.data$Rank==num),"Hospital.Name"]
               }   
      
       }else if (outcome == "pneumonia"){
             order.pneumonia=order(state.data$Pneumonia)
             state.data$Rank=NA
             state.data$Rank[order.pneumonia]=1:nrow(state.data)
             
             if(num == "best"){
               rank=1
               RankHospital=state.data[which(rank==1),"Hospital.Name"]
             } else if (num == "worst"){
               rank= max(state.data$Rank)
               RankHospital=state.data[which(state.data$Rank==rank),"Hospital.Name"]
             }else {
               num=as.integer(num)
               RankHospital=state.data[which(state.data$Rank==num),"Hospital.Name"]
             }   
         
       } else {
         stop("Invalid outcome")
         
       }
      
      RankHospital = as.character(RankHospital)
      return(RankHospital)
      
      
    }
}
  rankhospital("TX", "heart failure", 4)
