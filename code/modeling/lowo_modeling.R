# Leave one week out modeling framework
# How this works --
# For each week w:
# - filter out w as a holdout set
# - based on the remaining 8 weeks:
#   - train the participation model
#   - train the tackle probability model
# - based on week w:
#   - predict the probability of participation
#   - create the hypothetical position circles for points where a player has a "chance" at making a tackle
#   - determine the optimal position within that circle for the player to make a tackle
#   - save week's results
