# Stratified Cross-Validation for Uplift Modeling                     
# Author: Floris Devriendt (single treatment)                                                                    
# Modified by: Diego Olaya (three treatment alternatives)      

# The stratified.crossvalidation function returns a list of folds. Folds are stratified in relation to the ratio of three treatment groups. 
# Three treatment alternatives (variables): control, treatment_1 and treatment_2
# Outocome refers to a binary outcome variable 
# The amount of folds has to be specified

stratified.crossvalidation <- function(data,
                                       control, 
                                       treatment_1, 
                                       treatment_2, 
                                       outcome, 
                                       amount_folds){
  
  # We create six groups based on the three treatment alternatives and their respective outcome
  indices000 <- as.integer(rownames(data[data[,treatment_1] == 0 & data[,treatment_2] == 0 & data[, outcome] == 0,]))
  indices001 <- as.integer(rownames(data[data[,treatment_1] == 0 & data[,treatment_2] == 0 & data[, outcome] == 1,]))
  indices100 <- as.integer(rownames(data[data[,treatment_1] == 1 & data[,treatment_2] == 0 & data[, outcome] == 0,]))
  indices101 <- as.integer(rownames(data[data[,treatment_1] == 1 & data[,treatment_2] == 0 & data[, outcome] == 1,]))
  indices010 <- as.integer(rownames(data[data[,treatment_1] == 0 & data[,treatment_2] == 1 & data[, outcome] == 0,]))
  indices011 <- as.integer(rownames(data[data[,treatment_1] == 0 & data[,treatment_2] == 1 & data[, outcome] == 1,]))
  
  # The proportions of each group
  proportion000 <- nrow(data[indices000,])/nrow(data)
  proportion001 <- nrow(data[indices001,])/nrow(data)
  proportion100 <- nrow(data[indices100,])/nrow(data)
  proportion101 <- nrow(data[indices101,])/nrow(data)
  proportion010 <- nrow(data[indices010,])/nrow(data)
  proportion011 <- nrow(data[indices011,])/nrow(data)
  
  # The size takes into account the amount of folds
  # The remainder is stored when a clean division is not possible (i.e. with a remainder of 0)
  size000 <- length(indices000)%/%amount_folds
  remainder000 <- length(indices000)%%amount_folds
  
  size001 <- length(indices001)%/%amount_folds
  remainder001 <- length(indices001)%%amount_folds
  
  size100 <- length(indices100)%/%amount_folds
  remainder100 <- length(indices100)%%amount_folds
  
  size101 <- length(indices101)%/%amount_folds
  remainder101 <- length(indices101)%%amount_folds
  
  size010 <- length(indices010)%/%amount_folds
  remainder010 <- length(indices010)%%amount_folds
  
  size011 <- length(indices011)%/%amount_folds
  remainder011 <- length(indices011)%%amount_folds

  # Lists of extra remainders
  listExtra000 <- sample(1:amount_folds, remainder000, replace = FALSE)
  listExtra001 <- sample(1:amount_folds, remainder001, replace = FALSE)
  listExtra100 <- sample(1:amount_folds, remainder100, replace = FALSE)
  listExtra101 <- sample(x = 1:amount_folds, size = remainder101, replace = FALSE)
  listExtra010 <- sample(1:amount_folds, remainder010, replace = FALSE)
  listExtra011 <- sample(1:amount_folds, remainder011, replace = FALSE)
  
  # Empty fold-list
  fold_list <- list()
  
  # For each fold a sample is taken from each group of indices.
  # If an extra instance is stored in the fold (according to the listExtra as calculated above)
  # then the sample size is increased.
  # Each time a sample is taken from the groups of indices, the sampled indices are removed from the group.
  
  for(i in 1:amount_folds){
    # print(paste("Size of indices00: ", length(indices00)))
    # print(paste("Size of sample00: ", size00))
    #i <- 1

    # Sampling base set of indices
    sample000 <- sample(x = indices000,
                       size = if (i %in% listExtra000) (size000 + 1) else size000,
                       replace = FALSE)
    
    # Updating the indices
    indices000 <- setdiff(indices000,sample000)
    
    # --------------------------------------------------------------
    
    # Sampling base set of indices
    sample001 <- sample(x = indices001,
                       size = if (i %in% listExtra001) (size001 + 1) else size001,
                       replace = FALSE)
    
    # Updating the indices
    indices001 <- setdiff(indices001,sample001)
    
    # --------------------------------------------------------------
    
    # Sampling base set of indices
    sample100 <- sample(x = indices100,
                       size = if (i %in% listExtra100) (size100 + 1) else size100,
                       replace = FALSE)
    
    # Updating the indices
    indices100 <- setdiff(indices100,sample100)
    
    # --------------------------------------------------------------
    
    # Sampling base set of indices
    sample101 <- sample(x = indices101,
                       size = if (i %in% listExtra101) (size101 + 1) else size101,
                       replace = FALSE)
    
    # Updating the indices
    indices101 <- setdiff(indices101,sample101)
    
    # --------------------------------------------------------------
    
    # Sampling base set of indices
    sample010 <- sample(x = indices010,
                        size = if (i %in% listExtra010) (size010 + 1) else size010,
                        replace = FALSE)
    
    # Updating the indices
    indices010 <- setdiff(indices010,sample010)
    
    # --------------------------------------------------------------
    
    # Sampling base set of indices
    sample011 <- sample(x = indices011,
                        size = if (i %in% listExtra011) (size011 + 1) else size011,
                        replace = FALSE)
    
    # Updating the indices
    indices011<- setdiff(indices011,sample011)

    # --------------------------------------------------------------
        
    # Creating the list.
    fold_list[[i]] <- c(sample000, sample001, sample100, sample101, sample010, sample011)
  }
  
  return(fold_list)
}
