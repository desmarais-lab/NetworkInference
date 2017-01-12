# Function to calculate the edge weight, assuming exponential model
w_c = function(t_i,t_j,lambda,beta =0.5,epsilon=10^(-9),tied=FALSE) {
  # t_i is the time that i adopted
  # t_j is the time that j adopted
  # t_j > t_i
  
  score = ifelse(tied,log(beta*dexp(as.numeric(t_j)-as.numeric(t_i),lambda)), 
                 log(epsilon*dexp(as.numeric(t_j)-as.numeric(t_i),lambda)))
  #result is a scalar, on the log scale
  return(score)
}

# Function to find optimal spanning tree given empty graph and cascade
optimal_spanning_tree = function(cascade_data,lambda,beta =0.5,epsilon=10^(-9)) {
  # first column of cascade_data is the cascade id
  # second column of cascade_data is the vertex id
  # third column of cascade_data is the infection time
  
  # keep track of assigned parents
  parent = rep(NA,nrow(cascade_data))
  # keep track of the branch-wise score associated with each branch in the tree
  score = rep(NA,nrow(cascade_data))
  # loop over each infected node
  for(i in 1:nrow(cascade_data)) {
    # posible parents have times preceding the node's infection time
    # note, first infected node(s) not assigned a parent
    possible_parents = cascade_data[which(cascade_data[,3]<cascade_data[i,3]),2] # Should this be the 2nd col?
    # times of possible parents
    parent_times = cascade_data[which(cascade_data[,3]<cascade_data[i,3]),3]
    # store the scores associated with each parent
    parent_scores = numeric(length(possible_parents))
    # stop if there are no possible parents
    if(length(possible_parents)>0) {
      # loop over each parent
      for(j in 1:length(parent_scores)) {
        # calculate score for the jth parent
        parent_scores[j] = w_c(parent_times[j],cascade_data[i,3],lambda=lambda,beta =0.5,epsilon=10^(-9))
      }
      # store parent
      parent[i] = possible_parents[which.max(parent_scores)]
      # store associated score
      score[i] = parent_scores[which.max(parent_scores)]
    }
    
  }
  # returns a matrix in which the parent id and score is binded to the cascade_data
  return(cbind(cascade_data,parent,score))
}




# only need to do this once for a run of netinf
list_ordered_pairs = function(data) {
  require(combinat)
  # this function prepares a data structure that makes it easy to find cascades
  # u->v can only play a role if u is infected before v in the cascade
  # in which a given edge could be included in the tree
  
  # first column of data is the cascade id
  # second column of data is the vertex id
  # third column of data is the infection time
  
  # list unique cascade ids (big potential for paralellizing)
  cascades = unique(data[,1])
  # placeholder object to store the vertex ordering data
  ordering_data = NULL
  for(u in cascades) {
    cascade_data = data[data[,1]==u,]
    # find all pairs of nodes in the cascade
    all_pairs = t(combn(cascade_data[,2],2))
    # get infection times of each node in the pair
    timing1 = cascade_data[match(all_pairs[,1],cascade_data[,2]),3]
    timing2 = cascade_data[match(all_pairs[,2],cascade_data[,2]),3]
    # create vectors of the first node id and second node id
    first_node = ifelse(timing1 < timing2,all_pairs[,1],all_pairs[,2])
    second_node = ifelse(timing1 > timing2,all_pairs[,1],all_pairs[,2])
    # combine
    ordered_pairs = rbind(cbind(first_node,second_node))
    # remove ties
    ordered_pairs = rbind(ordered_pairs[timing1 != timing2,])
    # paste together into an ordered pair id
    pair_id = paste(ordered_pairs[,1],ordered_pairs[,2],sep="_")
    # cbind with cascade id and add to ordering data
    ordering_data = rbind(ordering_data,cbind(u,pair_id))
  }
  colnames(ordering_data) = c("cascade","pair_id")
  # reports the ordering data
  ordering_data
}

# function to initialize parent 

initialize_parents = function(data,lambda,beta =0.5,epsilon=10^(-9)) {
  # first column of data is the cascade id
  # second column of data is the vertex id
  # third column of data is the infection time
  
  # list cascades
  cascades = unique(data[,1])
  # vector in which to store the parent id
  parent = numeric(nrow(data))
  # vector in which to store scores associated with each branch
  score = numeric(nrow(data))
  # loop over cascades (could be parallelized)
  for(u in cascades) {
    # data associated with cascade u
    datau = subset(data,data[,1]==u)
    # find the best tree
    tree_result = optimal_spanning_tree(datau,lambda,beta,epsilon)
    # update parent id
    parent[which(data[,1]==u)] = tree_result[,4]
    # update score
    score[which(data[,1]==u)] = tree_result[,5]
  }
  # add parent id and score to data, then return
  return(cbind(data,parent,score))
}


# function to find the replacements and score for an edge
tree_replacements = function(parent_data,ordered_pairs,u,v,lambda,beta =0.5,epsilon=10^(-9)) {
  # parent data contains columns, cascade id, vertex id, infection time, current parent, score
  # ordered_pairs contains cascade id and 
  # tally up improvements
  improvement = 0
  # keep track of cascades in which the edge replaces existing branch
  replacements = NULL
  # track associated scores
  new_scores = NULL
  # create edge id to match to ordered pairs
  edgeid = paste(u,v,sep="_")
  # find cascades that could involve the edge
  cascades = ordered_pairs[which(ordered_pairs[,2]==edgeid),1]
  # loop over cascades
  for(s in cascades) {
    # timing of the potential parent
    timing_u = parent_data[which(parent_data[,1]==s & parent_data[,2]==u),3]
    # timing of the infected node
    timing_v = parent_data[which(parent_data[,1]==s & parent_data[,2]==v),3]
    # extract score associated with the current parent
    current_score = parent_data[which(parent_data[,1]==s & parent_data[,2]==v),5]
    # what would the score be with the propspective parent
    replacement_score =  w_c(timing_u,timing_v,lambda,beta,epsilon,tied=TRUE)
    # record as potential replcaement if replacement score is better than current
    if(as.numeric(replacement_score) > as.numeric(current_score)) {
      # add to total improvement across all cascades
      improvement = improvement + as.numeric(replacement_score) - as.numeric(current_score)
      # keep track of the cascades in which the edge would be included
      replacements = c(replacements,s)
      # record scores associated with the replacements
      new_scores = c(new_scores,replacement_score)
    }
  }
  return(list(improvement=improvement,replacement_data=cbind(replacements,new_scores)))
}


#' Pure R netinf implementation
#' 
#' Slow. For testing only.
#' 
#' @param cascade_data, a matrix with three columns (cascade id, then vertex id, 
#'     then infection time)
#' 
#' @export
netinfR = function(cascade_data,n.edges,lambda,beta =0.5,epsilon=10^(-9)) {
  
  # initialize spanning trees
  parent_data = initialize_parents(cascade_data, lambda,beta,epsilon)
  # get all the ordered pairs in the dataset
  ordered_pairs = list_ordered_pairs(cascade_data)
  # list posssible edges. note there is no way we will infer u->v if there
  # is no cascade in which u is infected before v
  possible_edges = unique(ordered_pairs[,2])
  # store edges and score associated with each new edge
  edges = character(n.edges)
  scores = numeric(n.edges)
  # find k edges
  for(k in 1:n.edges) {
    # keep tally of total score improvements offered by each edge
    improvements = numeric(length(possible_edges))
    # list of replacements offered by
    replacement_list = list()
    # index for storing data in improvements and replacement_list
    index = 1
    # loop over possible edges (could parallelize)
    for(e in possible_edges) {
      # split edge id into node ids
      uv = strsplit(e,"_")[[1]]
      # potential parent
      u = uv[1]
      # infected node
      v = uv[2]
      # find replacements for u->v edge
      e_replacements = tree_replacements(parent_data,ordered_pairs,u,v,lambda,beta,epsilon)
      # if there is at least one improvement, keep track of edge
      if(e_replacements$improvement > 0) {
        # store improvement
        improvements[index] = e_replacements$improvement
        # store all replacement information
        replacement_list[[index]] = e_replacements
        # name it to krrp track of edge
        names(replacement_list)[index] = e
        # increment index
        index = index + 1
      }
    }
    # extract best replacement
    replacement = replacement_list[[which.max(improvements)]]
    # store edge
    edges[k] = names(replacement_list)[which.max(improvements)]
    # store score
    scores[k] = max(improvements)
    # remove edge from list of possible edges
    possible_edges = possible_edges[-which(possible_edges==edges[k])]
    # extract nodes from edge id
    uv = strsplit(edges[k],"_")[[1]]
    u = uv[1]
    v = uv[2]
    # get data to update parent information for new edge
    replacement_data = replacement$replacement_data
    # loop over cascades to which new edge is added
    for(r in 1:nrow(replacement_data)) {
      # find row in parent data
      matching_row = which(parent_data[,1]==replacement_data[r,1] & parent_data[,2]==v)
      # update parent id
      parent_data[matching_row,4] = u
      # update branch score
      parent_data[matching_row,5] = replacement_data[r,2]
    }
  }
  return(list(edges=edges,scores=scores))
}
