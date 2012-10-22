best <- function(state, outcome) {
	## Read outcome data
	readFile = read.csv("outcome-of-care-measures.csv", colClasses = "character")
	State = readFile[, "State"]
	Hospital.Name = readFile[, "Hospital.Name"]
	Heart.Attack = readFile[, 11]
	Heart.Failure = readFile[, 17]
	Pneumonia = readFile[, 23]

	my.data = data.frame(Heart.Attack, Heart.Failure, Pneumonia, State, Hospital.Name)
	

	bool.state = my.data[, "State"] == state
	state.data = my.data[bool.state, ]
	state.data = na.omit(state.data)

	## sorted.best.data = state.data[order(Heart.Attack, Heart.Failure, Pneumonia, State, Hospital.Name), ]

	## Check that state and outcome are valid
	if(nrow(state.data) == 0){
		stop("Invalid state")
	}
	## Return hospital name in that state with lowest 30-day death
	## rate
	if(outcome == "heart attack"){
		minRate = min(state.data[,1])
		minHospital = state.data[which(state.data[,1] == minRate), "Hospital.Name"]
		

	}else if(outcome == "heart failure"){
		minRate = min(state.data[,2])
		minHospital = state.data[which(state.data[,2] == minRate), "Hospital.Name"]

	}else if(outcome == "pneumonia"){
		minRate = min(state.data[,3])
		minHospital = state.data[which(state.data[,3] == minRate), "Hospital.Name"]
	} else {
		stop("Invalid outcome")
	}
	return(minHospital)
	
}