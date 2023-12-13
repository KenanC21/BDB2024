# Considerations

- remove plays that were negated by penalties

- Be wary of hindsight bias with ghost defender approach. Ballcarrier's path is usually based on the location of the defender. If our model says "this defender should have been at location A rather than location B to have a better chance at making the tackle" it's possible that the ball carrier would have gone a different route if the defender was at location B.

- We only want to consider frames where there is a chance for a player to be tackled or pushed out of bounds. We'll call these frames our frames of interest.

- The frames of interest begin at each of the following events for a given play type:
  - reception: frame of the catch
  - handoff: frame of the handoff
  - QB run (undesigned): frame where player crosses LOS
  - QB run (designed): ?

- Only want to consider players who are realistically in the play (ex. If the ball is caught downfield, don't worry about defensive linemen)

- There are a few instances where a player is said to have missed a tackle on a play but never was even close to the ballcarrier on the play. In these cases we set the player's value to 0. This is typically very bad practice to change a target manually. However, since we are 100% positive that this was a data entry error and not a surprising result, we can confidently change the value.



# 
