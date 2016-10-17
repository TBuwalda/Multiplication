##########################################
###									   ###
###	Calculate Probability of retrieval ###
### Trudy Buwalda					   ###
### 11/10/16						   ###
###									   ###
##########################################
#rm(list = setdiff(ls(), lsf.str()))

sPar <- function(params) {
	return(sqrt(3) * params$activationNoise / pi)
}

tPar <- function(params) {
	return(sqrt(2) * sPar(params))
}

#associationStrengths <- matrix()
weights = 0
associationStrengths = 0


### Equations
##########################################
# Base-Level Learning Equation
baseLevelActivation <- function(chunk, params, chunks) {
	return(log(sum(chunks[chunks$name == chunk[[1]],]$timeSinceLastRef ^ (-params$decay))))
}

# Optimized Learning Equation
baseLevelActivationOptimized <- function(chunk, params, chunks) {
	return(log((chunks[chunks$name == as.character(chunk[[1]]),]$references * chunks[chunks$name == as.character(chunk[[1]]),]$lifetime ^ (-params$decay)) / (1 - params$decay)))
}

# Activation Equation
activation <- function(chunk, params, chunks) {
	if(params$optimizedLearning) {
		return(baseLevelActivationOptimized(chunk, params, chunks) + sum(weights*associationStrengths))
	} else {
		return(baseLevelActivation(chunk, params, chunks) + sum(weights * associationStrengths))
	}
}

# Match Equation
matchEquation <- function(request, chunk, params, chunks) {
	similarity <- calculateSimilarity(request, chunk, params)
	output <- activation(chunk, params, chunks) - params$mismatchPenalty * similarity
	return(output)
}

# Retrieval Probability Equaton
retrievalProbability <- function(request, params, chunks) {
	output <- NULL
	for(chunk in chunks$name) {
		tmp <- data.frame(chunk=chunk, 
			similarity = calculateSimilarity(request, chunks[chunks$name == chunk,c(1, 5:10)]), 
			activation = matchEquation(request, chunks[chunks$name == chunk,c(1, 5:10)], params, chunks), 
			probability = 1 / (1 + exp(-(matchEquation(request, chunks[chunks$name == chunk,c(1, 5:10)], params, chunks) - params$threshold) / sPar(params))))
		if(is.null(output)) {
			output <- tmp
		} else {
			output <- rbind(output, tmp)
		}
	}
	return(output)
}

# Chunk Choice Equation
chunkChoice <- function(request, params, chunks) {
	output <- NULL
	for(chunk in chunks$name) {
		counter <- exp(matchEquation(request, chunks[chunks$name == chunk, c(1, 5:10)], params, chunks) / tPar(params))
		tmp <- data.frame(chunk=chunk, counter=counter, probability=NA)
		if(is.null(output)) {
			output <- tmp
		} else {
			output <- rbind(output, tmp)
		}
	}
	output$probability <- output$counter / sum(output$counter)
	return(output)
}

# Prior Strength Equation
priorStrength <- function(source, chunk) {
	n <- sum(rowSums(chunks[,c(5:10)] == 8, na.rm=TRUE) != 0)
	m <- nrow(chunks)
	return(m / n)
}



calculateSimilarity <- function(request, chunk, params) {
	similarity = 0
	for(slot in 1:length(request)) {
		sim = 10
		if(request[[slot]] == chunk[[1 + slot]]) {
			sim = 0
		} else if(suppressWarnings(is.na(as.numeric(request[[slot]])))) {
			sim = -1
		} else {
			minvalue = min(request[[slot]], chunk[[1 + slot]])
			maxvalue = max(request[[slot]], chunk[[1 + slot]])
			if(params$similarityFunction == "linear") {
				sim = calculateSimilarityLinear(minvalue, maxvalue)
			} else if(params$similarityFunction == "christian") {
				sim = calculateSimilarityChristian(minvalue, maxvalue)
			} else {
				warning("Unknown Similarity Function")
				sim = -1
			}
		}
		similarity = similarity + abs(sim)
	}
	return(similarity)	
}

calculateSimilarityLinear <- function(minvalue, maxvalue) {
	sim = (as.numeric(minvalue) - as.numeric(maxvalue)) / 10	
	return(sim)	
}

calculateSimilarityChristian <- function(minvalue, maxvalue) {
	sim = (as.numeric(minvalue) / as.numeric(maxvalue)) - 1
	return(sim)	
}