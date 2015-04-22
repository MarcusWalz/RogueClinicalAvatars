# Run outcome metrics by mapping over simulation output using a special iterator.

# Why? To keep n small and things fast. Memory usage should be constant. 

# Think of this as a weird shoe factory. All the supplies to make only one shoe
# are sent in. Then the shoe exits the factory. Then more supplies go back in. 
# Until all the supplies run out.

# R becomes bottlenecked whenever there is too much supplies sitting around. By
# doing it this way we keep lookup times fast. So we have an O(n) algorithm instead
# of an O(n*log(n)) algorithm.

library(iterators) 
library(itertools)


# load avatars outputted by the sim.
avatar_iter <- function(files) { 
  ihasNext({
    avs = list()
    n <- function() {
      if(length(avs) == 0) {
        if(length(files) == 0) {
          stop("StopIteration")
        } else {
          avs <<- readRDS(files[1]) 
          files <<- files[-1]
        }
      }
      current_av = avs[[1]]
      avs <<- avs[-1]
      current_av
    }

    obj <- list(nextElem = n)
    class(obj) <- c('avatar_iter', 'abstractiter', 'iter')
    obj
})}


# map a function over an iterator
imap = function(iter, f) { 
  ihasNext({
    n = function(n) {
      f(nextElem(iter))
    }

    obj <- list(nextElem = n)
    class(obj) <- c('imap', 'abstractiter', 'iter')
    obj
})}


# take n items at a time
itake = function(iter, n) {
  ihasNext( { 
    my_next = function() {
      if(!hasNext(iter)) {
        stop("StopIteration")
      } else {
        out = list()
        turn = 0
        while(hasNext(iter) && turn < n) {
          out=append(out,list(nextElem(iter)))
          turn=turn+1
        }
      out
      }
    }
    obj <- list(nextElem = my_next)
    class(obj) <- c('itake', 'abstractiter', 'iter')
    obj
  } )
}

# store computations
isave = function(iter) {
  i = 0
  while(hasNext(iter)) {
    print(i)
    saveRDS(nextElem(iter), file=paste(i, "RDS", sep="."))
    i=i+1
  }
}

# convert an endpoint to an iter.
# name: is the name of the endpoin
# calculator: takes simulation output and returns the endpoint

create_endpoint = function(name, calculator) { 
  function(iter) { 
    imap(iter, function(sim_out) {
      sim_out$endpoint[name] = calculator(sim_out) 
      sim_out
    }) 
}

# compose multiple iterators into one
# TODO
icompose = NA

files = list.files(pattern="out.*.avatars.RData")

a_itr = avatar_iter(files)

test_f = function(x) { x$endpt$hello = 5; x }

t_itr = itake(imap(a_itr, test_f), 100)

isave(t_itr)
