## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## -----------------------------------------------------------------------------
library(ggplot2)


## -----------------------------------------------------------------------------
# Unique identifiers of each banana based on weight (and additional letter if weight tie)
banana_ids <- c(
  "130", "131", "140",
  "144", "146A", "146B",
  "147", "148", "155",
  "156", "157", "162",
  "166", "170", "171",
  "173", "175", "176",
  "181", "183A", "183B",
  "184", "185", "188",
  "189", "190", "200A",
  "200B", "209", "211"
)
# Banana weights
banana_weights <- c(
  130, 131, 140,
  144, 146, 146,
  147, 148, 155,
  156, 157, 162,
  166, 170, 171,
  173, 175, 176,
  181, 183, 183,
  184, 185, 188, 
  189, 190, 200,
  200, 209, 211
)
# Banana matched triplet assignments based on weight (group based on weight order statistics)
banana_match_ids <- unlist(lapply(1:10, function(n){rep(n,3)}))

banana_df <- data.frame(id = banana_ids, match_id = banana_match_ids, weight = banana_weights)

# Random treatment assignment with seed set for reproducibility
# 0 corresponds to control, 1 to paper bag, 2 to plastic bag
set.seed(123)
banana_df$treatment <- unlist(lapply(1:10, function(x){sample(0:2, 3, replace = FALSE)}))

banana_df


## -----------------------------------------------------------------------------
banana_rating <- read.csv("banana_ratings_final.csv")
# Reorder the banana_rating data frame by the BananaID
banana_rating <- banana_rating[order(banana_rating$BananaID),]

# Add a column to the banana_df data frame to store the post treatment rating, the values are the rows 1 through 30 of the Rating column in the banana_rating data frame
banana_df$post_rating <- banana_rating$Rating[1:30]

# Add a column to the banana_df data frame to store the difference between the pre treatment rating, the values are 31 through 60 of the Rating column in the banana_rating data frame
banana_df$pre_rating <- banana_rating$Rating[31:60]

# Add a column to the banana_df data frame to store the difference between the post treatment rating and the pre treatment rating
banana_df$rating_diff <- banana_df$post_rating - banana_df$pre_rating

banana_df


## -----------------------------------------------------------------------------
# Plot the distribution of the rating difference
ggplot(banana_df, aes(x = rating_diff)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Rating Difference", x = "Rating Difference", y = "Frequency")


## -----------------------------------------------------------------------------
ggplot(banana_df, aes(x = rating_diff, y = as.factor(treatment), fill = as.factor(treatment))) +
  geom_boxplot() + 
  labs(title = "Distribution of Rating Difference", 
       x = "Rating Difference", 
       y = "Frequency", 
       fill = "Group") +  # Change the legend title to "Group"
  scale_fill_manual(values = c("0" = "white", 
                               "1" = "lightblue", 
                               "2" = "brown"),   # Set colors for each group
                    labels = c("0" = "Control", 
                               "1" = "Plastic Bag", 
                               "2" = "Paper Bag")) +  # Map numeric values to labels
  theme_minimal()


## -----------------------------------------------------------------------------

# Permutaion Test 1: Control vs. Paper Bag


# Use the difference in means statistic to run a permutation test on control vs. paper bag
# Calculate the observed difference in means
obs_control_paper_diff <- mean(banana_df$rating_diff[banana_df$treatment == 1]) - mean(banana_df$rating_diff[banana_df$treatment == 0])

# Create a subset of the data frame with only the control and paper bag treatments
control_paper_df <- banana_df[banana_df$treatment %in% c(0, 1),]

# Create a vector to store the permuted differences in means
perm_diffs <- numeric(2^10)

# Run the permutation test by going through all possible permutations of the treatment assignments
# We are using the first 10 digits of the binary expansion of 
# the numbers 0 to 1023 to represent 
# all possible permutations of the 10 pairs
for(i in 0:(2^10 - 1)){
  # Convert the integer i to a binary representation
  binary_i <- as.integer(intToBits(i))
  
  # The length of binary_i is 32, but we only need the first 10 digits 
  # to represent the permutation of the 10 pairs
  binary_i <- binary_i[1:10]
  
  # Now, we can use binary_i to permute the data
  permuted_control_diff <- rep(NA, 10)
  permuted_paper_diff <- rep(NA, 10)
  
  for(j in 1:10){
    if(binary_i[j] == 0){
      permuted_control_diff[j] <- control_paper_df$rating_diff[j*2 - 1]
      permuted_paper_diff[j] <- control_paper_df$rating_diff[j*2]
    } else {
      permuted_control_diff[j] <- control_paper_df$rating_diff[j*2]
      permuted_paper_diff[j] <- control_paper_df$rating_diff[j*2 - 1]
    }
  }
  
  perm_diffs[i] <- mean(permuted_paper_diff) - mean(permuted_control_diff)
}

# Plot the distribution of the permuted differences in means
plot_1 <- ggplot() +
  geom_histogram(aes(x = perm_diffs), binwidth = 1, fill = "lightblue", color = "black") +
  geom_vline(xintercept = obs_control_paper_diff, color = "red") +
  labs(title = "Distribution of Permutated Differences in Means (Paper Bag - Control)", x = "Difference in Means", y = "Frequency")

# Calculate the p-value
p_value_1 <- sum(abs(perm_diffs) >= abs(obs_control_paper_diff)) / length(perm_diffs)


## -----------------------------------------------------------------------------

# Permutation Test 2: Control vs. Plastic Bag

# Use the difference in means statistic to run a permutation test on control vs. plastic bag
# Calculate the observed difference in means

obs_control_plastic_diff <- mean(banana_df$rating_diff[banana_df$treatment == 2]) - mean(banana_df$rating_diff[banana_df$treatment == 0])

# Create a subset of the data frame with only the control and plastic bag treatments
control_plastic_df <- banana_df[banana_df$treatment %in% c(0, 2),]

# Create a vector to store the permuted differences in means
perm_diffs <- numeric(2^10)

# Run the permutation test by going through all possible permutations of the treatment assignments
# We are using the first 10 digits of the binary expansion of
# the numbers 0 to 1023 to represent
# all possible permutations of the 10 twins

for(i in 0:(2^10 - 1)){
  # Convert the integer i to a binary representation
  binary_i <- as.integer(intToBits(i))
  
  # The length of binary_i is 32, but we only need the first 10 digits
  # to represent the permutation of the 10 pairs
  binary_i <- binary_i[1:10]
  
  # Now, we can use binary_i to permute the data
  permuted_control_diff <- rep(NA, 10)
  permuted_plastic_diff <- rep(NA, 10)
  
  for(j in 1:10){
    if(binary_i[j] == 0){
      permuted_control_diff[j] <- control_plastic_df$rating_diff[j*2 - 1]
      permuted_plastic_diff[j] <- control_plastic_df$rating_diff[j*2]
    } else {
      permuted_control_diff[j] <- control_plastic_df$rating_diff[j*2]
      permuted_plastic_diff[j] <- control_plastic_df$rating_diff[j*2 - 1]
    }
  }
  
  perm_diffs[i] <- mean(permuted_plastic_diff) - mean(permuted_control_diff)
}

# Plot the distribution of the permuted differences in means
plot_2<- ggplot() +
  geom_histogram(aes(x = perm_diffs), binwidth = 1, fill = "lightblue", color = "black") +
  geom_vline(xintercept = obs_control_plastic_diff, color = "red") +
  labs(title = "Distribution of Permutated Differences in Means (Plastic Bag - Control)", x = "Difference in Means", y = "Frequency")

# Calculate the p-value
p_value_2 <- sum(abs(perm_diffs) >= abs(obs_control_plastic_diff)) / length(perm_diffs)


## -----------------------------------------------------------------------------

# Permutation Test 3: Paper Bag vs. Plastic Bag


# Use the difference in means statistic to run a permutation test on paper bag vs. plastic bag
# Calculate the observed difference in means
obs_paper_plastic_diff <- mean(banana_df$rating_diff[banana_df$treatment == 2]) - mean(banana_df$rating_diff[banana_df$treatment == 1])

# Create a subset of the data frame with only the paper bag and plastic bag treatments
paper_plastic_df <- banana_df[banana_df$treatment %in% c(1, 2),]

# Create a vector to store the permuted differences in means
perm_diffs <- numeric(2^10)

# Run the permutation test by going through all possible permutations of the treatment assignments
# We are using the first 10 digits of the binary expansion of
# the numbers 0 to 1023 to represent
# all possible permutations of the 10 twins

for(i in 0:(2^10 - 1)){
  # Convert the integer i to a binary representation
  binary_i <- as.integer(intToBits(i))
  
  # The length of binary_i is 32, but we only need the first 10 digits
  # to represent the permutation of the 10 pairs
  binary_i <- binary_i[1:10]
  
  # Now, we can use binary_i to permute the data
  permuted_paper_diff <- rep(NA, 10)
  permuted_plastic_diff <- rep(NA, 10)
  
  for(j in 1:10){
    if(binary_i[j] == 0){
      permuted_paper_diff[j] <- paper_plastic_df$rating_diff[j*2 - 1]
      permuted_plastic_diff[j] <- paper_plastic_df$rating_diff[j*2]
    } else {
      permuted_paper_diff[j] <- paper_plastic_df$rating_diff[j*2]
      permuted_plastic_diff[j] <- paper_plastic_df$rating_diff[j*2 - 1]
    }
  }
  
  perm_diffs[i] <- mean(permuted_plastic_diff) - mean(permuted_paper_diff)
}

# Plot the distribution of the permuted differences in means
plot_3 <- ggplot() +
  geom_histogram(aes(x = perm_diffs), binwidth = 1, fill = "lightblue", color = "black") +
  geom_vline(xintercept = obs_paper_plastic_diff, color = "red") +
  labs(title = "Distribution of Permutated Differences in Means (Plastic Bag - Paper Bag)", x = "Difference in Means", y = "Frequency")

# Calculate the p-value
p_value_3 <- sum(abs(perm_diffs) >= abs(obs_paper_plastic_diff)) / length(perm_diffs)

