#' @title Icluster
#'
#' @description This function clusters the columns (variables) of a dataset via agglomerative hierarchical variable clustering using estimated multivariate similarities
#' (dependence coefficients) between random vectors.
#'
#' @param data The dataset (\eqn{n \times q} matrix with observations in rows, variables in columns) whose columns need to be clustered.
#' @param est_method The method for estimating the similarity between two clusters of variables.
#' @param max_dim The maximum dimension of the random vectors for which no link function is used when computing the similarity (default = Inf).
#' @param norm A possible normalization function applied to the dependence measure (default = NULL, meaning no normalization).
#' @param link The link function to be used when max_dim is exceeded (default = "average").
#'
#' @details
#' Suppose that the \eqn{q} variables (of which we have \eqn{n} observations in data) are \eqn{\mathcal{S} = \{X_{1}, \dots, X_{q}\}}.
#' Then, most important in hierarchical variable clustering, is computing the similarity
#' \deqn{\mathcal{D}(\mathbb{X},\mathbb{Y})} between two disjoint subsets of variables \eqn{\mathbb{X}, \mathbb{Y} \subset \mathcal{S}}.
#' In particular, the main algorithm is as follows:
#' \itemize{
#' \item Each object \eqn{\{X_{i}\}} forms a separate cluster, i.e., \eqn{\aleph_{1} = \{\{X_{1}\},\dots,\{X_{q}\}\}} is the initial feature partition.}
#' For \eqn{i = 1,2,\dots,q-1}:
#' \itemize{
#' \item For each pair of disjoint clusters \eqn{\mathbb{X},\mathbb{Y} \in \aleph_{i}}, compute the similarity \eqn{\mathcal{D}(\mathbb{X},\mathbb{Y})}.
#' \item Define \eqn{\aleph_{i+1} = (\aleph_{i} \setminus \{\widetilde{\mathbb{X}},\widetilde{\mathbb{Y}}\}) \cup \{\widetilde{\mathbb{X}} \cup \widetilde{\mathbb{Y}} \}}, where \eqn{\widetilde{\mathbb{X}},\widetilde{\mathbb{Y}}} are the clusters having maximal similarity according to the previous step.
#' \item The algorithm stops with \eqn{\aleph_{q} = \{\{X_{1},\dots,X_{q}\}\}}.}
#' We call \eqn{\{\aleph_{1}, \dots, \aleph_{q}\}} the hierarchy constructed throughout the algorithm, and define, for \eqn{i \in \{1, \dots, q\}},
#' \eqn{\text{Adiam}(\aleph_{i}) = |\aleph_{i}|^{-1} \sum_{\mathbb{X} \in \aleph_{i}} \text{diam}(\mathbb{X})}, with
#' \deqn{\text{diam}(\mathbb{X}) = \begin{cases} \underset{\{X,Y\} \subset \mathbb{X} }{\min} \mathcal{D}(X,Y)  & \mbox{if } |\mathbb{X}| > 1 \\ 1 & \mbox{if } |\mathbb{X}| = 1, \end{cases}}
#' and \eqn{\text{Msplit}(\aleph_{i}) = \max_{\mathbb{X} \in \aleph_{i}} \text{split}(\mathbb{X})}, with
#' \deqn{\text{split}(\mathbb{X}) =  \underset{Y \in \aleph_{i} \setminus \mathbb{X}}{\underset{X \in \mathbb{X}}{\max}} \mathcal{D}(X,Y) \hspace{0.2cm} \text{for} \hspace{0.2cm} \{\mathbb{X}\} \neq \aleph_{i}.}
#' Adiam stands for the average diameter of a partition (measuring the homogeneity, which we want to be large), while
#' Msplit stands for the maximum split of a partition (measuring the non-separation, which we want to be small).
#'
#' For measuring the similarity \eqn{\mathcal{D}(\mathbb{X},\mathbb{Y})}, we approach \eqn{\mathbb{X}} and \eqn{\mathbb{Y}} as being two random vectors
#' (let's say of dimensions \eqn{d_{1}} and \eqn{d_{2}} respectively). For \eqn{\mathcal{D}}, we take an estimated dependence measure between (two) random vectors. The following options are possible:
#' \itemize{
#' \item  list("phi", "mi", "Gauss", omegas = omegas) for the estimated Gaussian copula mutual information. Use omegas = 1 for no penalty, or a sequence of omegas for a ridge penalty tuned via 5-fold cross-validation,
#' see also the functions \code{\link{minormal}}, \code{\link{estR}}, and \code{\link{cvomega}},
#' \item list("phi", "Hel", "Gauss", omegas = omegas) for the estimated Gaussian copula Hellinger distance. Use omegas = 1 for no penalty, or a sequence of omegas for a ridge penalty tuned via 5-fold cross-validation,
#' see also the functions \code{\link{Helnormal}}, \code{\link{estR}}, and \code{\link{cvomega}},
#' \item list("phi", phi(t), "hac", type = type, M = M) for general \eqn{\Phi}-dependence with specified function phi(t) = \eqn{\Phi(t)},
#'        estimated by fitting (via pseudo maximum likelihood estimation) a hierarchical Archimedean copula of given type = type,
#'        and computed based on a Monte Carlo sample of size \eqn{M} in order to approximate the expectation, see also the functions \code{\link{mlehac}}, \code{\link{phihac}} and \code{\link{estphi}},
#' \item list("phi", phi(t), "nphac", estimator = estimator, type = type) for general \eqn{\Phi}-dependence with specified function phi(t) = \eqn{\Phi(t)},
#'        estimated via non-parametric beta kernel estimation or Gaussian transformation kernel estimation,
#'        and local bandwidth selection, by using a fitted (via pseudo maximum likelihood) hierarchical Archimedean copula
#'        as reference copula, see also the functions \code{\link{phinp}} and \code{\link{estphi}},
#' \item list("phi", phi(t), "np", estimator = estimator, bw_method = bw_method) for general \eqn{\Phi}-dependence with specified function phi(t) = \eqn{\Phi(t)},
#'         estimated via non-parametric beta kernel estimation or Gaussian transformation kernel estimation,
#'         and local bandwidth selection, either by using a non-parametric kernel estimator as reference copula if bw_method = 1,
#'         or by using a big O bandwidth rule if bw_method = 2, see also the functions \code{\link{phinp}} and \code{\link{estphi}},
#' \item list("phi", phi(t), "ellip", grid = grid) for general \eqn{\Phi}-dependence with specified function phi(t) = \eqn{\Phi(t)},
#'        estimated via the improved MECIP procedure on the specified grid, and parameter selection done via
#'        the function \code{\link{elliptselect}} using the Gaussian generator as reference generator, see also the functions \code{\link{phiellip}} and \code{\link{estphi}},
#' \item list("ot", coef = coef, omegas = omegas) for Gaussian copula Bures-Wasserstein dependence measures, either coefficient \eqn{\mathcal{D}_{1}} (coef = 1) or coefficient \eqn{\mathcal{D}_{2}} (coef = 2).
#'        Use omegas = 1 for no penalty, or a sequence of omegas for a ridge penalty tuned via 5-fold cross-validation,
#'        see also the functions \code{\link{bwd1}}, \code{\link{bwd2}}, \code{\link{estR}}, and \code{\link{cvomega}}.}
#'
#' When \eqn{d_{1} + d_{2} >} max_dim, the specified link function (say \eqn{L}) is used
#' for computing the similarity between \eqn{\mathbb{X}} and \eqn{\mathbb{Y}}, i.e.,
#' \deqn{\mathcal{D} \left ( \mathbb{X}, \mathbb{Y} \right ) = L \left (\left \{\mathcal{D}(X,Y) : X \in \mathbb{X}, Y \in \mathbb{Y} \right \} \right ),}
#' which by default is the average of all inter-pairwise similarities. Other options are "single" for the minimum, and "complete" for the maximum.
#'
#' The function norm (say \eqn{N}) is a possible normalization applied to the similarity measure, i.e., instead of
#' computing \eqn{\mathcal{D}} (using the method specified by est_method), the similarity becomes \eqn{N \circ \mathcal{D}}.
#' The default is \eqn{N(t) = t}, meaning that no normalization is applied.
#'
#' @return A list with elements "hierarchy" containing the hierarchy constructed throughout the algorithm (a hash object), "all" containing all similarities that were computed throughout the algorithm (a hash object),
#' "diam" containing the average diameters of all partitions created throughout the algorithm (a vector), and "split" containing the maximum splits of all partitions created throughout the algorithm (a vector).
#'
#' @references
#' De Keyser, S. & Gijbels, I. (2024).
#' Hierarchical variable clustering via copula-based divergence measures between random vectors.
#' International Journal of Approximate Reasoning 165:109090.
#' doi: https://doi.org/10.1016/j.ijar.2023.109090.
#'
#' De Keyser, S. & Gijbels, I. (2024).
#' Parametric dependence between random vectors via copula-based divergence measures.
#' Journal of Multivariate Analysis 203:105336.
#' doi: https://doi.org/10.1016/j.jmva.2024.105336.
#'
#' De Keyser, S. & Gijbels, I. (2024).
#' High-dimensional copula-based Wasserstein dependence.
#' doi: https://doi.org/10.48550/arXiv.2404.07141.
#'
#' @seealso \code{\link{minormal}} for the computation of the Gaussian copula mutual information,
#'          \code{\link{Helnormal}} for the computation of the Gaussian copula Hellinger distance,
#'          \code{\link{estphi}} for several approach to estimating the \eqn{\Phi}-dependence between \eqn{k} random vectors,
#'          \code{\link{bwd1}} for the computation of the first Bures-Wasserstein dependence coefficient \eqn{\mathcal{D}_{1}},
#'          \code{\link{bwd2}} for the computation of the second Bures-Wasserstein dependence coefficient \eqn{\mathcal{D}_{2}}.
#'
#' @examples
#' q = 20
#' dim = c(2,3,3,5,7) # We will impose a clustering {{X1,X2},{X3,X4,X5},{X6,X7,X8},{X9,X10,X11,X12,X13},{X14,X15,X16,X17,X18,X19,X20}}
#'
#' n = 200 # Sample size
#'
#' hac = gethac(dim,c(2,3,4,5,6,7),type = 1) # Twenty dimensional hierarchical Gumbel copula with parameters
#' # (theta_0,theta_1,theta_2,theta_3,theta_4,theta_5) = (2,3,4,5,6,7)
#' # So, strongest cluster is {X14,X15,X16,X17,X18,X19,X20}, then {X9,X10,X11,X12,X13},
#' # then {X6,X7,X8}, then {X3,X4,X5}, and finally {X1,X2}
#' sample = HAC::rHAC(n,hac) # Sample
#'
#' # Cluster using different methods
#'
#' # Gaussian copula based methods
#'
#' Clustering1 =  Icluster(data = sample, est_method = list("phi", "mi", "Gauss", omegas = 1))
#' Clustering1$hierarchy$Aleph_16 # 5-cluster partition
#'
#' Clustering2 =  Icluster(data = sample, est_method = list("phi", "mi", "Gauss", omegas = seq(0.01,0.999,len = 50)))
#' Clustering2$hierarchy$Aleph_16 # 5-cluster partition
#'
#' Clustering3 =  Icluster(data = sample, est_method = list("phi", "mi", "Gauss", omegas = 1), max_dim = 2)
#' Clustering3$hierarchy$Aleph_16 # 5-cluster partition
#'
#' Clustering4 =  Icluster(data = sample, est_method = list("phi", "Hel", "Gauss", omegas = 1))
#' Clustering4$hierarchy$Aleph_16 # 5-cluster partition
#'
#' Clustering5 =  Icluster(data = sample, est_method = list("ot", coef = 1, omegas = 1))
#' Clustering5$hierarchy$Aleph_16 # 5-cluster partition
#'
#' Clustering6 =  Icluster(data = sample, est_method = list("ot", coef = 2, omegas = 1))
#' Clustering6$hierarchy$Aleph_16 # 5-cluster partition
#'
#' Clustering7 =  Icluster(data = sample, est_method = list("ot", coef = 2, omegas = 1), max_dim = 4)
#' Clustering7$hierarchy$Aleph_16 # 5-cluster partition
#'
#' # Parametric hierarchical Archimedean copula approach
#'
#' Clustering8 = Icluster(data = sample, est_method = list("phi", function(t){t * log(t)}, "hac", type = 1, M = 1000), max_dim = 4)
#' Clustering8$hierarchy$Aleph_16 # 5-cluster partition
#'
#' Clustering9 = Icluster(data = sample, est_method = list("phi", function(t){(sqrt(t)-1)^2}, "hac", type = 1, M = 1000), max_dim = 2)
#' Clustering9$hierarchy$Aleph_16 # 5-cluster partition
#'
#' # Non-parametric approaches
#'
#' Clustering10 = Icluster(data = sample, est_method = list("phi", function(t){t * log(t)}, "nphac", estimator = "beta", type = 1), max_dim = 3)
#' Clustering10$hierarchy$Aleph_16 # 5-cluster partition
#'
#' Clustering11 = Icluster(data = sample, est_method = list("phi", function(t){t * log(t)}, "nphac", estimator = "trans", type = 1), max_dim = 3)
#' Clustering11$hierarchy$Aleph_16 # 5-cluster partition
#'
#' Clustering12 = Icluster(data = sample, est_method = list("phi", function(t){t * log(t)}, "np", estimator = "beta", bw_method = 1), max_dim = 3)
#' Clustering12$hierarchy$Aleph_16 # 5-cluster partition
#'
#' Clustering13 = Icluster(data = sample, est_method = list("phi", function(t){t * log(t)}, "np", estimator = "trans", bw_method = 2), max_dim = 3)
#' Clustering13$hierarchy$Aleph_16 # 5-cluster partition
#'
#' Clustering14 = Icluster(data = sample, est_method = list("phi", function(t){(sqrt(t)-1)^2}, "np", estimator = "trans", bw_method = 1), max_dim = 2)
#' Clustering14$hierarchy$Aleph_16 # 5-cluster partition
#'
#' # Semi-parametric meta-elliptical copula approach
#'
#' Clustering15 = Icluster(data = sample, est_method = list("phi", function(t){t * log(t)}, "ellip", grid = seq(0.005,100,by = 0.005)), max_dim = 2)
#' Clustering15$hierarchy$Aleph_16 # 5-cluster partition
#'
#' @export

Icluster = function(data, est_method, max_dim = Inf, norm = NULL, link = "average"){

  start_time = Sys.time() # Time the algorithm

  if(is.null(norm)){norm = function(t){t}} # Identity normalization if none is given

  # Function sim is the similarity measure used throughout the algorithm

  sim = function(sample, dim, est_method){

    # Computes the similarity between the columns of sample corresponding to dimensions specified in dim
    # Method will be "No link" in the algorithm
    # Est_method is as specified in the function Icluster, and will be the argument for est_PHI function

    if(est_method[[1]] == "phi"){ # If (normalized) Phi-dependence measures are used

      if(is.character(est_method[[2]]) && est_method[[2]] == "mi"){ # If Gaussian copula mutual information is used

        if(length(est_method[[4]]) == 1){ # If omegas = 1 (no penalization)

          return(norm(minormal(estR(sample,est_method[[4]]),dim))) # Mutual information

        } else{

          omega = cvomega(sample,est_method[[4]],5) # Select omega via 5-fold cross-validation
          return(norm(minormal(estR(sample,omega),dim))) # Mutual information

        }
      }

      else if(is.character(est_method[[2]]) && est_method[[2]] == "Hel"){ # If Gaussian copula Hellinger distance is used

        if(length(est_method[[4]]) == 1){  # If omegas = 1 (no penalization)

          return(norm(Helnormal(estR(sample,est_method[[4]]),dim))) # Hellinger distance

        } else{

          omega = cvomega(sample,est_method[[4]],5) # Select omega via 5-fold cross-validation
          return(norm(Helnormal(estR(sample,omega),dim))) # Hellinger distance

        }

      } else{ # If other method using est_PHI function

        return(norm(estphi(sample,dim,est_method[-c(1,2)],est_method[[2]]))) # Phi-dependence

      }
    }

    else if(est_method[[1]] == "ot"){ # If optimal transport measures are used

      if(length(est_method[[3]]) == 1){ # If omegas = 1 (no penalization)

        if(est_method[[2]] == 1){ # If D1 is to be used

          return(norm(bwd1(estR(sample,est_method[[3]]),dim))) # D1

        } else{

          return(norm(bwd2(estR(sample,est_method[[3]]),dim))) # D2

        }

      } else{

        omega = cvomega(sample,est_method[[3]],5)  # Select omega via 5-fold cross-validation

        if(est_method[[2]] == 1){ # If D1 is to be used

          return(norm(bwd1(estR(sample,omega),dim))) # D1

        } else{

          return(norm(bwd2(estR(sample,omega),dim))) # D2

        }
      }
    }
  }

  # Initialization

  q = ncol(data)
  n = nrow(data)
  colnames(data) = integer(q)

  for(j in 1:q){ # Name the columns of data X1,X2,...,Xq
    name = paste("X",toString(j),sep = "")
    colnames(data)[j] = name
  }

  hierarchy = sets::set(sets::as.set(colnames(data))) # Initial hierarchy is {{X1,X2,...,Xq}}
  all = hash::hash() # Empty hash object to store all similarities that are computed
  cluster = sets::as.set(colnames(data)) # Initial clustering (partition) of all variables {X1,...,Xq}
  STC_computed = sets::set() # Empty set for storing similarities that are already computed
  finish = 0 # Index indicating when algorithm terminates
  index = 1 # Index indicating which partition has been found
  stepp = 1 # Index keeping track of the amount of bivariate similarities that are computed

  # Main algorithm

  while(finish != 1){

    names_cluster = c() # Vector to store all names of the current partition

    # If for example cluster = sets::set(c("X34"),c("X2"),c("X3","X4"),c("X25","X1","X22")) = {X34,X2,c(X3,X4),c(X25,X1,X22)}
    # then names_cluster = c("X34","X2","(X3,X4)","(X1 X22 X25)"), not necessarily in this order,
    # but within a cluster, variables are sorted according to index

    for(i in cluster){names_cluster = c(names_cluster,set_to_string(sets::set(i)))}

    # Next, we order the names according to the first appearing index in each cluster, i.e. for the example
    # names_cluster = c("(X1 X22 X25)","X2","(X3,X4)","X34")

    names_cluster = unlist(strsplit(set_to_string(sets::as.set(names_cluster)), "(\\((?:[^()]++|(?1))*\\))(*SKIP)(*F)| ", perl = TRUE))

    # Names for all pairwise combinations of which similarities need to be computed are contained in
    # names_combn, e.g. for the example, names_combn = c("(X1 X22 X25) X2","(X1 X22 X25) (X3 X4)","(X1 X22 X25) X34","X2 (X3 X4)","X2 X34")

    names_combn = apply(combn(names_cluster,2), 2, paste, collapse = " ")

    # All possible combinations are also contained in the set combn, e.g. for the example
    # combn = {{X2,X34},{X2,c(X3,X4)},{X2,c(X25,X1,X22)},{X34,c(X3,X4)},{X34,c(X25,X1,X22)},{c(X3,X4),c(X25,X1,X22)}}
    # Recall that set elements have no order

    combn = sets::set_combn(cluster,2)

    # Combinations that have not yet been considered are stored in STC_new, with a total of length(STC_new)

    STC_new = sets::set_complement(STC_computed,combn)
    length_STC_new = length(STC_new)

    count = 1 # Index keeping track of how many similarities of length(STC_new) are already computed

    for(set in STC_new){ # Take a couple for which the similarity is to be computed, e.g. {x2,c(X3,X4)}

      str_set = set_to_string(set) # Unique string representation, e.g. "X2 (X3 X4)"
      dim = c() # Vector for dimensions of two clusters contained in set
      sample = c() # Vector (matrix) for sample data of two clusters contained in set

      for(i in set){ # For each of the two clusters, e.g. X2 and c(X3,X4)

        dim = c(dim,length(i)) # Concatenate dimensions, e.g. c(1,2)
        sample = cbind(sample,data[,i,drop = F]) # Concatenate sample data (recall that colnames(data) = X1,...,Xq)

      }

      names_to_keep = colnames(sample) # Keep the names of the current sample in consideration
      colnames(sample) = NULL # Then remove colnames of sample

      # In case ncol(sample) == 2, we have two univariate random variables

      if(ncol(sample) == 2){

        S = sim(sample,dim,est_method) # Compute bivariate similarity
        print(paste("Bivariate similarity ", stepp, " out of ", choose(q,2), " is found"))
        stepp = stepp + 1 # Update stepp index

      }

      # In case ncol(sample) > max_dim, use link function to compute similarity

      else if(ncol(sample) > max_dim){

        S = link(names_to_keep,all,dim,link = link) # Compute link similarity

      } else{ # Or compute multivariate similarity

        S = sim(sample,dim,est_method)
        print(paste(ncol(sample), "dimensional similarity is found"))

      }

      all[[str_set]] = S # Update hash object, where key = unique string representation of the two clusters
      # values = similarity between the two clusters

      if(choose(q,2) != length_STC_new){ # In case we are not in the first iteration of the algorithm
        # where only bivariate similarities are computed

        print(paste("Similarity", count, " out of ", length_STC_new, " is found"))
        count = count + 1 # Update count index

      }
    }

    # Val_all is a named numerical vector with names the unique string representations and
    # values the corresponding similarity

    val_all = unlist(as.list(all))
    val_all = val_all[names(val_all) %in% names_combn] # Only consider those computed in the current iteration

    # Take the ones with largest similarity, e.g. "X2 (X3 X4)" and convert back to a set, e.g. {X2, c(X3,X4)}

    best_set = string_to_set(names(val_all[val_all == max(val_all)])[1])

    # Put all variables included in best_set in a vector ordered according to index

    best_vct = gtools::mixedsort(set_to_vct(best_set))

    # A new partition is obtained by removing best_set and adding the merged two clusters
    # e.g. remove x2 and c(X3,X4) and add c(X2,X3,X4)

    cluster = sets::set_union(sets::set_symdiff(cluster,best_set),sets::set(best_vct))

    # Add the new partition to the hierarchy

    hierarchy = sets::set_union(hierarchy,sets::set(cluster))

    print(paste("Partition of ", q - index , " elements is found"))

    STC_computed = sets::set_union(STC_new,STC_computed) # Update already computed similarities

    index = index + 1 # Update index

    if(length(cluster) == 2){ # Stop merging when only two clusters remain (can be altered)

      finish = 1

    }
  }

  end_time = Sys.time()
  print(difftime(end_time, start_time, units='mins')) # Print total running time

  # Finalization

  # Complete the hierarchy by adding the final partition (consisting of all variables in one single cluster)

  hierarchy = sets::set_union(hierarchy,sets::set(sets::set(colnames(data))))

  # We summarize all partitions in a hash object with keys (aleph1 = first partition, .... alephd = d'th partition)
  # and values the unique string representation of the respective partition

  new_hierarchy = hash::hash()
  pos = 0

  for(set in hierarchy){

    set = sort_set(set)
    name = paste("Aleph_",length(hierarchy) - pos, sep = "")
    new_hierarchy[[name]] = set_to_string(set)
    pos = pos + 1

  }

  # We compute the average diameters of all partitions

  avg_diameters = integer(length(hierarchy))
  pos = 1

  for(set in hierarchy){

    avg_diameters[pos] = avg_diam(set,all)
    pos = pos + 1

  }

  # We compute the maximum splits of all partitions

  max_splits = integer(length(hierarchy)-1)
  pos = 1

  for(set in hierarchy){

    if(length(set) != 1){

      max_splits[pos] = max_split(set,all)
      pos = pos + 1

    }
  }

  end_time = Sys.time()
  print(difftime(end_time, start_time, units='mins')) # Print total running time

  # Return a list containing the "hierarchy" (hash object), "all" similarities computed in the algorithm (hash)
  # the average diameters "diam" (vector) and the maximum splits "split" (vector)

  return(list("hierarchy" = new_hierarchy, "all" = all, "diam" = avg_diameters, "split" = max_splits))

}

# Auxiliary functions

sort_set = function(set){

  # This function sorts all vectors in set
  # Each vector of the form c("Xi1",...,"Xik") gets sorted according to i1,...,ik
  # and converted to a string of the form "(Xj1 ... Xjl)"

  for(i in set){

    if(length(i) > 1){

      set = sets::set_union(sets::set_symdiff(set,sets::set(i)),sets::set(paste("(",paste(i[order(as.numeric(gsub('X', '', i)))],collapse=" "),")",sep = "")))

    }
  }

  return(set)

}

set_to_vct = function(set){

  # Puts set elements in a vector

  vct = c()

  for(i in set){

    vct = c(vct,i)

  }

  return(vct)

}

set_to_string = function(set){

  # Gives unique string representation of a partition by first sorting within each group,
  # and converting to string vector notation (using sort_set) and then sorting the groups
  # according to their first X index
  # Returns one string representing the clustering
  # Example: sets::set(c("X34"),c("X2"),c("X3","X4"),c("X25","X1","X22"))
  # gives "(X1 X22 X25) X2 (X3 X4) X34"

  vect = set_to_vct(sort_set(set))

  for(i in 1:length(vect)){

    if(substr(vect[i],1,1) == "("){

      vect[i] = substring(vect[i], 2)

    }
  }

  firsts = c()

  for(i in 1:length(vect)){

    firsts = c(firsts, sub(" .*", "", vect[i]))

  }

  order = order(as.numeric(gsub('X', '', firsts)))
  vect = vect[order]

  for(i in 1:length(vect)){

    if(gsub(" ", "",vect[i], fixed = TRUE) != vect[i]){

      vect[i] = paste("(",vect[i], sep = "")

    }
  }

  return(paste(vect, collapse = ' '))

}

string_to_set = function(string){

  # Reverse operation of set_to_string

  string = unlist(strsplit(string, "(\\((?:[^()]++|(?1))*\\))(*SKIP)(*F)| ", perl=TRUE))
  set = sets::as.set(NULL)

  for(i in 1:length(string)){

    set = sets::set_union(set,sets::set(unlist(strsplit(gsub("\\(|\\)", "", string[i]), "\\s+"))))

  }

  return(set)

}

avg_diam = function(partition, all){

  # Average diameter of a partition

  diameters = integer(length(partition))
  index = 1

  for(i in partition){

    if(length(i) == 1){

      min_s = 1

    } else{

      STC = sets::set_combn(i,2)
      S_values = integer(length(STC))
      pos = 1

      for(set in STC){

        str_set = set_to_string(set)
        S_values[pos] = all[[str_set]]
        pos = pos + 1

      }

      min_s = min(S_values)

    }

    diameters[index] = min_s
    index = index + 1

  }

  return(mean(diameters))

}

max_split = function(partition, all){

  # Maximum split of a partition

  splits = integer(length(partition))
  index = 1

  for(i in partition){

    rest = sets::set_symdiff(partition,sets::set(i))
    S_values = c()

    for(j in i){

      for(k in rest){

        for(l in k){

          str_set = set_to_string(sets::set(j,l))
          S_values = c(S_values,all[[str_set]])

        }
      }
    }

    splits[index] = max(S_values)
    index = index + 1

  }

  return(max(splits))

}

## Link function used in Icluster algorithm

link = function(names, all, dim, link){

  # This function computes the similarity between two groups by taking the average, single
  # or complete linkage of all pairwise inter similarities already computed before and contained in all
  # Names contains the variable names of the two groups

  d1 = ifelse(dim[1] == 1,1,dim[1])
  d2 = ifelse(dim[2] == 1,1,dim[2])
  dep = matrix(0,d1,d2)

  for(i in 1:d1){

    for(j in 1:d2){

      if(d1 == 1){

        name1 = names[1]

      } else{

        name1 = names[i]

      }

      if(d2 == 1){

        name2 = names[length(names)]

      } else{

        name2 = names[dim[1] + j]

      }

      dep[i,j] = all[[set_to_string(sets::set(name1,name2))]]

    }
  }

  if(link == "average"){

    return(mean(dep))

  }

  if(link == "single"){

    return(min(dep))

  }

  if(link == "complete"){

    return(max(dep))

  }
}

convert_to_hclust = function(Iclust){

  # This function converts the Iclust object (output from Icluster functions) to an hclust object

  a = list()
  hier = Iclust$hierarchy
  l = length(hier)
  merge = matrix(0,nrow = l-1, ncol = 2)
  clusters = list()

  for(i in 1:l){

    clusters[[i]] = i

  }

  for(k in 1:(l-1)){

    name_old = paste("Aleph_",k,sep = "")
    name_new = paste("Aleph_",k+1,sep = "")
    old = as.set(unlist(strsplit(hier[[name_old]], "(\\((?:[^()]++|(?1))*\\))(*SKIP)(*F)| ", perl=TRUE)))
    new = as.set(unlist(strsplit(hier[[name_new]], "(\\((?:[^()]++|(?1))*\\))(*SKIP)(*F)| ", perl=TRUE)))
    change = set_to_vct(sets::set_complement(old,new))
    match = as.numeric(unlist(regmatches(change, gregexpr("[[:digit:]]+", change))))

    for(i in 1:(length(clusters)-1)){

      for(j in (i+1):length(clusters)){

        if((sum(match %in% c(clusters[[i]],clusters[[j]])) == length(match))){

          new_cluster = sort(c(clusters[[i]],clusters[[j]]))

          if(i <= l && j <= l){

            merge[k,] = c(-i,-j)

          }

          if(i <= l && j > l){

            merge[k,] = c(-i,j-l)

          }

          if(i > l && j <= l){

            merge[k,] = c(i-l,-j)

          }

          if(i > l && j > l){

            merge[k,] = c(i-l,j-l)

          }
        }
      }
    }

    clusters[[l+k]] = new_cluster

  }

  a$merge = merge
  a$height = 1:(l-1)
  a$order = 1:l
  names = integer(l)

  for(j in 1:l){

    name = paste("X",toString(j),sep = "")
    names[j] = name

  }

  a$labels = names
  class(a) = "hclust"

  return(a)

}

plot_indices = function(Iclust, k){

  # This function plots a dendrogram of an Iclust object (output from Icluster function)
  # It also plots the average diameter and maximum split as a function of the number of clusters
  # k is the number of colors used in the dendrogram

  dev.new()
  l = length(Iclust$split) + 1
  diams = Iclust$diam[2:l]
  splits = Iclust$split
  d = as.dendrogram(convert_to_hclust(Iclust))

  d %>% dendextend::set("leaves_pch", 19) %>%
    dendextend::set("leaves_cex", 0.4) %>%
    dendextend::set("leaves_col", "black") %>%
    dendextend::set("branches_k_color","black") %>% # k = k for colors
    dendextend::set("labels_cex", 0.7) %>%
    plot(main = "Dendrogram", cex.main = 2.5, axes = F)
  abline(h = 56, lty = 2)
  # text(-6, 90, "6", xpd = NA)
  # dev.new()

  frame = as.data.frame(cbind(seq(1,l),Iclust$diam[1:l]))

  g = ggplot(frame) +
    geom_line(aes(x = V1, y = V2), size = 1, col = "black") +
    xlab("Number of classes") + ylab("Average diameter") +
    scale_x_continuous(breaks= pretty_breaks()) +
    theme_bw() + theme(axis.text = element_text(size=15),
                       axis.title = element_text(size=20))
  # print(g)

  frame = as.data.frame(cbind(seq(2,l),splits))
  # dev.new()

  g = ggplot(frame) +
    geom_line(aes(x = V1, y = splits), size = 1, col = "black") +
    xlab("Number of classes") + ylab("Maximum split") +
    scale_x_continuous(breaks= pretty_breaks()) +
    theme_bw() + theme(axis.text = element_text(size=15),
                       axis.title = element_text(size=20))
  # print(g)
  # dev.new()

  frame = as.data.frame(cbind(seq(2,l),1-diams,splits))
  names(frame) = c("NoC","diams","splits")
  # ggplot(frame) + geom_point(aes(x = diams, y = splits), col = "black", size = 3) +
  #  geom_text(aes(x = diams, y = splits, label = ifelse(NoC <= 10,NoC," ")),
  #            hjust = 0.5, vjust = -0.8, size = 5) +
  #  geom_segment(aes(xend = diams, yend = splits), x = 0, y = 0, col = 'black', lty = "dotted") +
  #  xlab("1-Average diameter") + ylab("Maximum split") + xlim(0,1) + ylim(0,1) +
  #  theme_bw() + theme(axis.text = element_text(size=15),
  #                     axis.title = element_text(size=20))
}

Silhouette = function(Iclust){

  # Silhouette index (based on dissimilarity = 1 - similarity)

  q = length(Iclust$hierarchy)
  all = Iclust$all
  meanss = integer(q)

  for(p in 1:q){

    print(paste("p equals", p, "out of", q))
    part = Iclust$hierarchy[[paste("Aleph_",toString(p),sep = "")]]
    part = string_to_set(part)
    ais = integer(length(q))
    bis = ais
    indexais = 1
    indexbis = 1

    for(i in part){

      l = length(i)

      if(l == 1){

        ais[indexais] = 0 # zero dissimilarity
        indexais = indexais + 1

      } else{

        for(j in 1:l){

          Dis_values = integer(l-1)
          pos = 1

          for(k in setdiff(1:l,j)){

            Dis_values[pos] = 1 - all[[set_to_string(as.set(c(i[j],i[k])))]]
            pos = pos + 1
          }

          ais[indexais] = mean(Dis_values)
          indexais = indexais + 1

        }
      }

      if(l == q){

        bis = rep(Inf,q)

      } else{

        for(j in 1:l){

          Dis = integer(length(part)-1)
          rest = set_symdiff(part,sets::set(i))
          pos1 = 1

          for(k in rest){

            Dis_values = integer(length(k))
            pos2 = 1

            for(m in k){

              Dis_values[pos2] = 1 - all[[set_to_string(as.set(c(i[j],m)))]]
              pos2 = pos2 + 1

            }

            Dis[pos1] = mean(Dis_values)
            pos1 = pos1 + 1

          }

          bis[indexbis] = min(Dis)
          indexbis = indexbis + 1

        }
      }
    }

    ss = integer(q)

    for(i in 1:q){

      if(ais[i] == 0 || bis[i] == Inf){

        ss[i] = 0

      } else{

        ss[i] = (bis[i] - ais[i])/max(ais[i],bis[i])

      }
    }

    meanss[p] = mean(ss)

  }

  return(meanss)

}

Redundancy = function(data, Iclust, coef = "mi", est_method = "Gauss", omegas = NULL, phi = NULL, norm = NULL){

  # Multivariate redundancy

  if(is.null(norm)){norm = function(t){t}} # Identity normalization if none is given

  q = ncol(data)
  colnames(data) = integer(d)

  for(j in 1:q){ # Name the columns of data X1,X2,...,Xq

    name = paste("X",toString(j),sep = "")
    colnames(data)[j] = name

  }

  if(is.null(omegas)){

    omega = 1

  } else{

    omega = cvomega(data,omegas,5)

  }

  red = integer(q-1) # Redundancy

  for(i in 1:(q-1)){

    print(paste("i = ", i, "out of", q-1))
    current_partition = Iclust$hierarchy[[paste("Aleph_",toString(i),sep = "")]]
    nclusters = q-i+1
    dim_clusters = integer(nclusters)
    sample = c()
    index = 1

    for(j in string_to_set(current_partition)){

      dim_clusters[index] = length(j)
      sample = cbind(sample,data[,j])
      index = index + 1

    }

    if(est_method[[1]] == "Gauss"){

      R_est = estR(sample,omega)

      if(coef == "mi"){

        red[i] = norm(minormal(R_est,dim_clusters))

      }

      if(coef == "Hel"){

        red[i] = norm(Helnormal(R_est,dim_clusters))

      }

      if(coef == "bwd1"){

        red[i] = norm(bwd1(R_est,dim_clusters))

      }

      if(coef == "bwd2"){

        red[i] = norm(bwd2(R_est,dim_clusters))

      }

    } else{

      red[i] = norm(estphi(sample,dim_clusters,est_method,phi))

    }
  }

  dev.new()
  frame = as.data.frame(cbind(seq(2,q),red))

  g = ggplot(frame) +
    geom_line(aes(x = V1, y = rev(red)), size = 1, col = "black") +
    xlab("# clusters") + ylab("Redundancy") +
    scale_x_continuous(breaks= pretty_breaks()) +
    theme_bw() + theme(axis.text = element_text(size=15),
                       axis.title = element_text(size=20),
                       plot.title = element_text(hjust=0.5, size = 25))
  print(g)

  return(frame)

}

RI = function(optimal, part, adj = FALSE){

  # Computes the (adjusted, if adj = TRUE) rand index between the partition optimal
  # (target partition) and the partition part (computed partition)

  picked = integer(length(optimal))
  nclust = length(unique(optimal))
  part = unlist(strsplit(part, "(\\((?:[^()]++|(?1))*\\))(*SKIP)(*F)| ", perl = TRUE))

  for(i in 1:nclust){

    ind = as.numeric(unlist(regmatches(part[i], gregexpr("[[:digit:]]+", part[i]))))
    picked[ind] = i

  }

  if(adj == TRUE){

    return(adj.rand.index(optimal,picked))

  } else{

    return(rand.index(optimal,picked))

  }
}
