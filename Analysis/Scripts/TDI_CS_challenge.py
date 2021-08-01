# Design choices:
# For the grid, I'm going to use a convention that allows for calculation of distance easily
# A step can be +1 or -1 in three directions, so a position is a tuple (x, y, z)
# But there are three rules and their negative versions:
# (1, 0, 1) = (0, 1, 0)
# (1, -1, 0) = (0, 0, -1)
# (0, 1, -1) = (1, 0, 0)
# This rule gets enforced due to parity: i.e. 1st and 3rd axes cannot be same sign, while
# 1st and 2nd, as well as 2nd and 3rd axes has to be the same sign
import numpy as np
from functools import lru_cache
ALLSTEPS = [(1, 0, 0), (-1, 0, 0), (0, 1, 0), (0, -1, 0), (0, 0, 1), (0, 0, -1)]

# This function returns the resolved location after taking a step of size 1 from current position
def resolve_step(cur_pos, step_dir):
    if (abs(step_dir[0]) == 1):
        # take a step in the first axis
        updated = step_dir[0] + cur_pos[0]
        # if the first axis is diff. sign as second, or same sign as third, enforce the resolving rule
        if abs(updated) > 0 and ((np.sign(updated) * -1 == np.sign(cur_pos[1])) or (np.sign(updated) == np.sign(cur_pos[2]))):
            return (cur_pos[0], cur_pos[1] + step_dir[0], cur_pos[2] - step_dir[0])
        else:
            return (updated, cur_pos[1], cur_pos[2])
    elif (abs(step_dir[1]) == 1):
        # take a step in the second axis
        updated = step_dir[1] + cur_pos[1]
        # if the second axis is diff. sign as first or third, enforce the resolving rule
        if abs(updated) > 0 and ((np.sign(updated) * -1 == np.sign(cur_pos[0])) or (np.sign(updated) * -1 == np.sign(cur_pos[2]))):
            return (cur_pos[0] + step_dir[1], cur_pos[1], cur_pos[2] + step_dir[1])
        else:
            return (cur_pos[0], updated, cur_pos[2])
    elif (abs(step_dir[2]) == 1):
        # take a step in the third axis
        updated = step_dir[2] + cur_pos[2]
        # if the third axis is same sign as first or diff. sign as second, enforce the resolving rule
        if abs(updated) > 0 and ((np.sign(updated) == np.sign(cur_pos[0])) or (np.sign(updated) * -1 == np.sign(cur_pos[1]))):
            return (cur_pos[0] - step_dir[2], cur_pos[1] + step_dir[2], cur_pos[2])
        else:
            return (cur_pos[0], cur_pos[1], updated)
    else:
        print('Need a valid step of size 1!')
        return None

# This function returns the list of all six resolved locations if one step is taken from the current position
# use cache decorator to scale better
# It was possible to run T=60 without caching on my personal computer, but caching results in significant speedup
@lru_cache(maxsize = None)
def take_step(cur_pos):
    # Take 6 steps
    return [resolve_step(cur_pos, x) for x in ALLSTEPS]

# Our design allows for easy calculation of distance
def get_dist(cur_pos):
    return abs(cur_pos[0]) + abs(cur_pos[1]) + abs(cur_pos[2])

# Now we need a data structure for saving the state at T = i
# The design choice here is to use a dictionary, with the hashable location tuple as key,
# and the number of 'leaves in the traversal tree' as the value.
# We'll say the starting position is (0, 0, 0), so initial state is {(0, 0, 0): 1}
old_state = {(0, 0, 0): 1}
# running_dict keeps the whole history - kept for debugging purposes
running_dict = {0: old_state}
steps = 13
for i in range(steps):
    new_state = {}
    for k,v in old_state.items():
        leaves = take_step(k)
        for l in leaves:
            if l not in new_state:
                new_state[l] = 0
            new_state[l] += v
    running_dict[i+1] = new_state
    old_state = new_state

# All answers are rounded to 5 decimal places
# Q1 and Q2: mean and variance from the full distribution at T = 13
mean = sum([v*get_dist(k) for k,v in new_state.items()]) / (6**steps)
var = sum([v*((get_dist(k) - mean)**2) for k,v in new_state.items()]) / (6**steps)
sd = np.sqrt(var)

print(mean)
print(sd)

# Q5: conditional mean
cond_sum = 0
N_tot = 0
for k,v in new_state.items():
    if (get_dist(k)) >= 4:
        cond_sum += (v * get_dist(k))
        N_tot += v

cd_mean = cond_sum / N_tot
print(cd_mean)

# Also run until T = 60, but this time don't save the running_dict
old_state = {(0, 0, 0): 1}
# running_dict keeps the whole history
# running_dict = {0: old_state}
steps = 60
for i in range(steps):
    new_state = {}
    for k,v in old_state.items():
        leaves = take_step(k)
        for l in leaves:
            if l not in new_state:
                new_state[l] = 0
            new_state[l] += v
#     running_dict[i+1] = new_state
    old_state = new_state

# Q3 and Q4: mean and variance from the full distribution at T = 60
mean = sum([v*get_dist(k) for k,v in new_state.items()]) / (6**steps)
var = sum([v*((get_dist(k) - mean)**2) for k,v in new_state.items()]) / (6**steps)
sd = np.sqrt(var)

print(mean)
print(sd)

# Q6: conditional probability
cond_N_tot = 0
N_tot = 0
for k,v in new_state.items():
    if (get_dist(k)) >= 15:
        N_tot += v
    if (get_dist(k)) >= 20:
        cond_N_tot += v

cd_prob = cond_N_tot / N_tot
print(cd_prob)