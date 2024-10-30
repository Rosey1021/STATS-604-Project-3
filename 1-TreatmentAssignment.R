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