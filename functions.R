# This function is the forward algorithm
# Input:
# n = number of states
# seq = sequence
# Output: log forward probabilities for corresponding sequence position and state
forward <- function(n, seq, transitions, emissions)
{
  # create an empty matrix 
  # dimension = n+2 x length(seq)+1, 
  # where n = number of states (add two for begin and end states)
  logProbMatrix = matrix(data = rep(0, (n+2)*(length(seq)+1)), nrow = (n+2), ncol = (length(seq)+1))
  
  # Initialize (1,1) to 1
  logProbMatrix[1,1] = 1
  
  # Fill in logProbMatrix Top-Down, Left-Right
  for(j in 2:ncol(logProbMatrix))
  {
    for(i in 2:nrow(logProbMatrix))
    {
      # Compute probability of generating first j chars and ending in state i
      # 1. Find out how many states point to state i
      pointerInfo = findPointers(transitions, i)
      
      sum = 0
      
      for(k in 1:pointerInfo$numPointers)
      {
        currSum = logProbMatrix[pointerInfo$pointers[k],j-1] * pointerInfo$transProb[k]
        sum = sum + currSum
      }
      
      # 2. find emissionProb for current state
      emitProb = emissionProb(emissions, i-1, seq[i-1])
      
      # 3. compute log prob
      logProbMatrix[i,j] = log(emitProb*sum)
    }
  }
  
  return(logProbMatrix)
}

# This function returns necessary emission probability
# Input:
# emissions = table of emission probabilities
# stateID = state ID
# symbol = emission symbol
# Output:
emissionProb <- function(emissions, stateID, symbol)
{
  return(emissions[emissions$statNum == stateID & emissions$emissionSymb == symbol,3])
}

# For current state, 
# output the number of pointers to state, 
# the states that point to currState,
# and the transition probabilities.
findPointers <- function(transitions, currState)
{
  return(list(numPointers = length(transitions[transitions$to == currState,1]), 
         pointers = transitions[transitions$to == currState,1],
         transProb =  transitions[transitions$to == currState,3]))
}








