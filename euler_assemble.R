# Run this R script with the following arguments:
# Rscript euler_assemble.R k_num.kmers.txt
# or
# sh euler_assemble.sh k_num.kmers.txt

# Clear the workspace
rm(list = ls())

# This package uses the insert() function
suppressMessages(library(R.utils))

#################### Problem 1 #################### 

# Get command line arguments
args = commandArgs(trailingOnly = TRUE)

# split file name by "/" and "_" where the first numeric value represents k
#k = as.numeric(strsplit(strsplit(args[1], split = "/")[[1]][2], split = "_")[[1]][1])
#k = as.numeric(strsplit(args[1], split = "_")[[1]][1])
file = data.frame(read.table(args[1]))

# Convert file to string characters
file[,1] = as.character(file[,1])

# Convert file into a list of kmers
kmers = file[[1]]

# Determine k value since reading it from the file name isn't working
k = length(strsplit(kmers[1], split = '')[[1]])

# Order list of kmers by lexicographic order
kmers = kmers[order(kmers)]

# Load functions_reverse_complement.R
source("euler_assemble_functions.R")

superString = buildPath(k, kmers)
as.name(superString)


