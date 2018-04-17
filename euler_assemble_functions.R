# This function builds a de bruijn graph based on inputs
buildDeBuijnGraph <- function(k, kmerList)
{
  # For each kmer, we need to split into k-1 prefix and k-1 suffix terms
  # So create a dataframe of this
  prefixSuffixDF = data.frame(prefix = character(), suffix = character(),
                              stringsAsFactors = FALSE)
  varName = colnames(prefixSuffixDF)
  
  # Fill prefixSuffixDF with prefix and suffix terms
  for(i in 1:length(kmerList))
  {
    # split ith element in kmerList into prefix and suffix
    prefix = paste(strsplit(kmerList[i], split = "")[[1]][1:k-1], collapse = '')
    prefix
    suffix = paste(strsplit(kmerList[i], split = "")[[1]][2:k], collapse = '')
    
    # add prefix and suffix to prefixSuffixDF
    prefixSuffixString = c(prefix, suffix)
    prefixSuffixDF = rbind(prefixSuffixDF, prefixSuffixString, stringsAsFactors = FALSE)
  }
  
  # Change back column names of prefixSuffixDF back to prefix and suffix
  colnames(prefixSuffixDF) = varName
  
  # Create an adjacency matrix based on
  # unique elements in prefixSuffixDF.
  # Note: rows represent prefix and cols represent suffix
  uniqueVertices = unique(unlist(prefixSuffixDF))[order(unique(unlist(prefixSuffixDF)))]
  adjacencyMatrix = matrix(rep(0, length(uniqueVertices)*length(uniqueVertices)), 
                           nrow = length(uniqueVertices), ncol = length(uniqueVertices))
  colnames(adjacencyMatrix) = uniqueVertices
  rownames(adjacencyMatrix) = uniqueVertices
  
  # Go through prefixSuffixDF and add counts to adjacencyMatrix
  for(i in 1:nrow(prefixSuffixDF))
  {
    # get current prefix and suffix value
    currPrefix = prefixSuffixDF$prefix[i]
    currSuffix = prefixSuffixDF$suffix[i]
    
    # Add a count to adjacencyMatrix that corresponds to
    # currPrefix and currSuffix
    adjacencyMatrix[currPrefix, currSuffix] = adjacencyMatrix[currPrefix, currSuffix] + 1
  }
  return(adjacencyMatrix)
}

# Function determines if a given path is a cycle
isCycle <- function(path)
{
  # Base case = the path is empty
  if(length(path) == 0 || length(path) == 1)
  {
    return(FALSE)
  }
  return(path[1] == path[length(path)])
}

# Function determines if cycle is Eulerian
isEulerian <- function(graph)
{
  return(all(graph == 0))
}

# This function takes as input an adjacency matrix,
# starts at the vertex that is 1st in lexi order
# and returns a cycle and updated graph
findCycle <- function(graph, mergedCycle)
{
  # Initialize a candidate path
  path = c()
  updatedGraph = graph
  
  # Go through adjacency matrix (graph) and add to path
  # Rules:
  # 1. Choose the vertex (row) that is 1st in lexi order
  # 2. For every iteration besides the first, start the cycle
  # at the first vertex within prevCycle that has an unused outgoing edge.
  currVertex = ""
  if(is.null(mergedCycle))
  {
    # Find first vertex with non zero value in updatedGraph
    currVertex = rownames(updatedGraph)[which(t(updatedGraph) != 0, arr.ind = TRUE)[1,2]]
  } else{
    # subset updatedGraph to only include vertices in mergedCycle
    subGraph = updatedGraph[mergedCycle,]
    
    # Find first vertex with non zero value in subGraph
    currVertex = rownames(subGraph)[which(t(subGraph) != 0, arr.ind = TRUE)[1,2]]
  }
  
  # Add current vertex to graph
  path = c(path, currVertex)
  
  # While path is not a cycle
  while(!isCycle(path))
  {
    ## Find the first vertex s.t. currVertex -> vertex
    ## i.e. the first non zero column in row for currVertex
    
    # Move to next non-zero vertex
    nextVertex = colnames(updatedGraph)[which(updatedGraph[currVertex,] != 0, arr.ind = TRUE)[1]]
    
    # Update path and graph
    path = c(path, nextVertex)
    updatedGraph[currVertex, nextVertex] = updatedGraph[currVertex, nextVertex] - 1
    
    # Set currVertex to nextVertex
    currVertex = nextVertex
  }
  
  # return cycle and updated graph
  result = list(newCycle = path, graph = updatedGraph)
  return(result)
  
}

# This function merges the new and old cycle in current iteration
mergeCycles <- function(newCycle, oldMergedCycle)
{
  # Merge cycle with previous cycle
  # To merge:
  # 1. Base case: oldCycle is empty
  if(is.null(oldMergedCycle))
  {
    return(newCycle)
  }else{
    # Find occurence of first vertex in newCycle in oldMergedCycle
    firstVertex = newCycle[1]
    
    # Find index of first occurence
    idx = match(firstVertex, oldMergedCycle)
    
    # Insert new cycle into old merged cycle
    return(insert(oldMergedCycle, ats = idx, values = newCycle[1:(length(newCycle)-1)]))
  }
}

eulerianPathFindingAlg <- function(graph)
{
  # Initialize previous cycle, merged cycle, and 
  # current graph
  prevCycle = c()
  mergedCycle = c()
  currGraph = graph
  
  # First iteration: 
  # 1. Find a cycle and get updated grqph
  currCycleAndGraph = findCycle(currGraph, mergedCycle)
  currCycle = currCycleAndGraph$newCycle
  currGraph = currCycleAndGraph$graph 
  
  # 2. Merge currCycle and prevCycle
  mergedCycle = mergeCycles(currCycle, prevCycle)

  # update prevCycle with currCycle
  prevCycle = currCycle
  itr = 2
  # 2nd iteration and so on...
  while(!isEulerian(currGraph))
  {
    # Find a cycle and get updated grqph
    currCycleAndGraph = findCycle(currGraph, mergedCycle)
    currCycle = currCycleAndGraph$newCycle
    currGraph = currCycleAndGraph$graph 
    
    # merge currCycle and prevCycle
    mergedCycle = mergeCycles(currCycle, mergedCycle)
    
    # update prevCycle with currCycle
    prevCycle = currCycle
    itr = itr + 1
  }
  
  return(mergedCycle)
}

# This function finds unbalanced vertices
# Input: adjacency matrix
# Output: pair of vertices that are unbalanced
findUnbalancedVertices <- function(graph)
{
  prefixSuffixDF = data.frame(prefix = character(), suffix = character(),
                              stringsAsFactors = FALSE)
  varName = colnames(prefixSuffixDF)
  
  # A graph is balanced if indegree = outdegree for all nodes
  for(i in 1:nrow(graph))
  {
    # Check if sum of row = sum of col for each node
    outdegree = sum(graph[i,])
    indegree = sum(graph[,i])
    
    if(indegree == outdegree)
    {
      # If theyre the same => balanced so continue search
      # by going to next iteration
      next
    }
    # If indegree > outdegree, then this vertex must be my
    # new prefix to add to since I want an additional 
    # edge pointing out of it
    else if(indegree > outdegree)
    {
      # record ith node name in graph to prefixSuffixDF
      prefixSuffixDF[1,1] = rownames(graph)[i]
    }
    # else indegree < outdegree, then this vertex must be my
    # new suffix to add to since I want an additional 
    # edge pointing in it
    else
    {
      prefixSuffixDF[1,2] = rownames(graph)[i]
    }
  }
  return(prefixSuffixDF)
}

# Function to see if graph is balanced
isBalanced <- function(graph)
{
  # Initialize balanced to TRUE
  balanced = TRUE
  
  # A graph is balanced if indegree = outdegree for all nodes
  for(i in 1:nrow(graph))
  {
    # Check if sum of row = sum of col for each node
    outdegree = sum(graph[i,])
    indegree = sum(graph[,i])
    
    if(indegree != outdegree)
    {
      balanced = FALSE
      break
    }
  }
  return(balanced)
}

# This function combines superString according to kmer size
# given that there are no "fake" edges
combineCyclic <- function(superString){
  result = paste(rep("", 1), collapse = "")
  
  # Add the first element to every other one except the end
  for(i in 1:(length(superString)-1))
  {
    # Get first character of each element in your superString
    stringSplit = strsplit(superString[i], split = '')
    letter = stringSplit[[1]][1]
    
    # Add letter to result
    result = paste(result, letter, sep = "")
  }
  
  # Add the last whole element in superString to the end
  result = paste(result, superString[length(superString)], sep = "")
  
  return(result)
}

# This function combines superString according to kmer size
# given that there are "fake" edges
combineAcyclic <- function(superString, unbalancedVertices){
  result = paste(rep("", 1), collapse = "")
  
  # Start at first occurence of suffix
  prefix = unbalancedVertices$prefix
  suffix = unbalancedVertices$suffix
  
  # Search for end index for fake edge
  # e.g. if AG -> TG, then return index for TG
  idx = 0
  for(i in 1:(length(superString)-1))
  {
    if((superString[i] == prefix) && (superString[i+1] == suffix))
    {
      # return index number i
      # Note: break once we find the first occurrence
      # (according to the algorithm given)
      idx = i+1
      break
    }
  }
  
  # Reorder superString
  firstPartOfNewString = superString[idx:length(superString)]
  
  # update superString then delete first element
  secondPartOfNewString = superString[1:(idx-1)]
  # Check if you remove all elements if you removed the first
  if(identical(secondPartOfNewString[-1], character(0)))
  {
    # then superString is just the first part
    superString = firstPartOfNewString
  } else{
    # remove first element (since it is the same as the end of the cycle)
    secondPartOfNewString = secondPartOfNewString[-1]
    
    # create new superstring
    superString = c(firstPartOfNewString, secondPartOfNewString)
  }
  
  # Add the first element to every other one except the end
  for(i in 1:(length(superString)-1))
  {
    # Get first character of each element in your superString
    stringSplit = strsplit(superString[i], split = '')
    letter = stringSplit[[1]][1]
    
    # Add letter to result
    result = paste(result, letter, sep = "")
  }
  
  # Add the last whole element in superString to the end
  result = paste(result, superString[length(superString)], sep = "")
  
  return(result)
}

# This function builds the superstring and returns to main program
buildPath <- function(k, kmerList)
{
  # First build a de Bruijn graph
  graph = buildDeBuijnGraph(k, kmerList)
  
  # If graph is not symmetric => not balanced
  if(!isBalanced(graph))
  {
    # Find the pair of unbalanced vertices
    unbalancedVertices = findUnbalancedVertices(graph)
    
    # Edit graph so that there is an additional count to new graph
    # based on unbalancedVertices
    # Go through prefixSuffixDF and add counts to graph
   
    # get current prefix and suffix value
    currPrefix = unbalancedVertices$prefix
    currSuffix = unbalancedVertices$suffix
      
    # Add a count to graph that corresponds to
    # currPrefix and currSuffix
    graph[currPrefix, currSuffix] = graph[currPrefix, currSuffix] + 1
    
    # Run Eulerian path finding algorithm
    superString = eulerianPathFindingAlg(graph)
    superString = combineAcyclic(superString, unbalancedVertices)
    # Remove the "fake edge" you placed, then start
    # superstring from there
    superString = paste(superString, collapse = '')
    return(superString) 
    
  }else{
    # Run Eulerian path finding algorithm
    superString = eulerianPathFindingAlg(graph)
    superString = combineCyclic(superString)
    superString = as.name(paste(superString, collapse = ''))
    return(superString) 
  }
}



