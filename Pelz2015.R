# attention switching model from PelzPiantadosiKidd ICDL 2015

# parameters:
# (A) the learning curve that the learner follows as he attends to the stimulus, 
# (B) memory decay rate, as a model of the learner’s short term memory limitations, 
# (C) the cost of switching attention between objects (0.5), and 
# (D) the amount of information that the learner knows about the objects at the start of test. 

# Attention allows accumulation of data about an object, but there is a diminishing return 
# as the learner comes to know everything that can be learned about each object. 
# Importantly, these objects are best thought of as dynamic stimuli that will change when unattended, 
# meaning that information will be lost about them when attention is oriented elsewhere.

# At each timestep, the model computes the amount of information that it expects to gain from each object, 
# and chooses whether to continue to attend to the current object or to switch to another based on the 
# expected information gains

# Gompertz learning curve for currently attended object
info_gain <- function(t, a=100, b=1, c=1) {
  # returns amount of information known about each object at time t, 
  # a is the maximum amount of information that can be learned from each object (fixed at 100 bits), 
  # b characterizes the amount of initial information, 
  # c is the learning rate (fixed at 1.0 for simplicity)
  return( a*exp(-b*exp(-c*t)) )
}

plot_info_gain <- function() {
  timesteps = seq(0,5, .01)
  info = unlist(lapply(timesteps, info_gain, b=5))
  plot(x=timesteps, y=info, main="Learning curve for attended stimulus",
       ylab="Information (bits)")
}

#plot_info_gain()

# power law decay of information for unattended objects
decay <- function(t, beta=.1) {
  # t = amount of time since object was last attended
  # beta = information decay parameter
  return( (t+1)^(-beta) )
}

plot_decay <- function() {
  timesteps = seq(0,5, .01)
  info = unlist(lapply(timesteps, decay))
  plot(x=timesteps, y=info, main="Information decay for unattended stimuli",
       ylab="Percent Information")
}

#plot_decay()

# softmax to determine P(choosing to attend obj i)
softmax <- function(obj_info, chi=30) {
  # chi=0 - random choice, chi=Inf - perfect maximization; 
  # chi=30 corresponds to a 95% chance of choosing an option that gives .1 bits more than its alternative
  obj_info_norm = obj_info / sum(obj_info)
  choice_prob = exp(chi*obj_info_norm) / sum(exp(chi*obj_info_norm))
  return(choice_prob) # probability of choosing each object
}

# expected information gain of attending to each object (including switch cost)
exp_info_gain <- function(obj_info, t, switch_cost=.5) {
  gain = info_gain(t) - obj_info - switch_cost # NEED TO NOT SUBTRACT SWITCH COST FOR ATTENDED OBJ!
  return(gain)
}

#softmax(c(1,1,2), chi=5)

run_sim <- function(obj_start_info, timesteps=seq(0,5,.1)) {
  nobj = length(obj_start_info) # number of objects
  info_traj = matrix(0, nrow=length(timesteps), ncol=nobj) # information per object over time
  info_traj[1,] = obj_start_info
  for(t in 2:length(timesteps)) {
    eig = exp_info_gain(info_traj[t-1,], t)
    choice_prob = softmax(eig)
    attended_obj = sample(1:nobj, prob=choice_prob, 1)
    unattended_objs = setdiff(1:nobj, attended_obj)
    info_traj[t,attended_obj] = info_gain(timesteps[t]) # maybe multiplied or added to info_traj[t-1] ?
    info_traj[t,unattended_objs] = info_traj[t-1,unattended_objs] * decay(timesteps[t])
  }
  colnames(info_traj) = paste("Object",1:nobj)
  return(info_traj)
}

sim1 = run_sim(c(2,2,4))
sim2 = run_sim(c(1,1,1))
