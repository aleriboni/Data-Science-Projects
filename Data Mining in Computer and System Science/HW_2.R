################################# DAMI Programming Assignment 2: Clustering ##################################

## IMPORTANT! In this document, when you see [FIXME], that is where you should fill in your corresponding R code! 

## If would like to implement every part by yourself, in your submissions, you just comment out the provided codes, and write down your own codes in the corresponding place instead!

## IMPORTANT! Download the synthetic data "cluster.RData" from the course website. This data need to be used throughout the assignment!
## Save the data to your current working directory


# Load the data in your R environment
# This should give you dataframe "cluster.data" 
load("cluster.RData")

###### Part 1: Distance Calculation ######
# TASK 1. Writing a function for distance calculation (implement the formula):

# parameters "a" and "b" are two vectors 
# Manhattan distance is calculated between "a" and "b" when metric = "manhattan"
# otherwise Euclidean distance is calculated and returned by the function
# NOTE: Implement the equation for distance calculation
my_dist_calculator <- function(a, b, metric = "euclidean"){
  
  if(metric == "manhattan"){
    # write code for calculating manhattan distance, use the formula
    mydistance <- sum(abs(a - b))
    
  }else{
    # write code for calculating euclidean distance, use the forumula
    mydistance <- sqrt(sum((a - b)^2))
  }
  
 return(mydistance) 
}

# Test your function on these two vectors
a = c(4.2, 7.1)
b = c(3.1, 5.8)

my_dist_calculator(a, b, "manhattan")
my_dist_calculator(a, b, "euclidean")
# result should be: manhattan -> 2.4, euclidean -> 1.702939



###### Part 2: K-Means Clustering #######
# TASK 2a. Write a function for performing K-Means clustering

# K-means steps
# 1. Assign random cluster ID (from 1 to K) to each observation
# 2. Compute mean (centroid) of observations belonging to each K clusters
# 3. Compute distance of each observations from all K centroids
# 4. Reassign each observation to a cluster with nearest centroid (reassign cluster ID)
# 5. Repeat from step 2 to 4 until cluster ID do not change or maximum iteration reached


# Function takes parameter "x" as dataframe to be clustered
# "k" as a number of clusters and "max.iter" as maximum allowed iteration before convergence
# Use the Euclidean distance calculator from part 1.

k_means <- function(x, k, max.iter = 20){
  
  # each observation is assigned a random cluster (id) from 1 to k
  random_index <- sample(1:k, nrow(x), replace = TRUE)
  
  # add a new column "cluster" to dataset (x) that carries cluster id for each observation
  data_with_cluster <- cbind(x, clusterID = random_index)
  head(data_with_cluster)
  # initialize number of iteration counts to 1
  # this is used to stop iterating when maximum limit is reached
  iterations = 1
  
  # plot the data points
  plot(data_with_cluster[,1:2])
  
  # this block of code tries to reassign new cluster ID to observations
  # based on which cluster centroid it is nearest to
  # repeat the process until the clusterID for observations do not change
  # or number of interation reach maximum limit
  while(TRUE){
    
    # create a new object "centroids" to hold the mean (centroid) of each cluster
    # It is a matrix whose number of rows equals to k (number of clusters)
    # and number of columns equals number of columns in original data (x)
    # initialize this matrix with 0 values
    centroids <- matrix(rep(0, times = k * ncol(x)), nrow = k, ncol = ncol(x))
    
    # compute the mean of observations in each cluster and save them in "centroids"
    for(i in 1:k){
      # find the rowids of observations in cluster i, use which() function
      obs_of_cluster_i <- which(data_with_cluster$clusterID == i)
      
      # find the column mean (use colMeans()) of observations with rowids = "obs_of_cluster_i"
      # and save in corresponding row in "centroids" matrix
      centroids[i,] <- colMeans(x[obs_of_cluster_i,])

    }
    
    # ---------------------------------------------- #
    # plots the centroids discovered in each iteration
    points(centroids[,1:2], pch = 20, cex = 2)
    # waits until some charater is fed
    # this will help to track changing centroids in each iteration
    readline(prompt = "Press Enter to continue:")
    # ---------------------------------------------- #
    
    # Calculate an euclidean distance of each observation from all k centroids
    # Distance matrix "dist_from_centroids" has as many rows as x and k columns
    # to store distances
    # First column holds distances of each observations from first centroid
    # and similarly second and third column has distance from corresponding centroids.
    dist_from_centroids <- matrix(rep(0, nrow(x) * k), nrow = nrow(x), ncol = k)
    
    # Compute the distance between centroid and each observation 

    for(i in 1:nrow(x)){
      for(j in 1:nrow(centroids)){
        # Use the euclidean distance calculation function created in TASK 1.
        dist_from_centroids[i,j] <- my_dist_calculator(x[i,],centroids[j,])
      }
    }
    
    # from the distance matrix computed, find for each observation the closest cluster centroid
    obs_new_clusterID <- apply(dist_from_centroids, 1, which.min)
    
    # If the centroid is not changing any more for each observation, stop the iteration
    if(all(obs_new_clusterID == data_with_cluster$clusterID)){ 
      km.clusters <- obs_new_clusterID
      centroid.matrix <- centroids
      break
      # If number of iterations exceed the maximum iterations allowed, stop the iteration
    }else if(iterations > max.iter){
      break
      
      # Otherwise, assign the new centroids to the dataset, and continue the iteration
    }else{ 
      data_with_cluster$clusterID <- obs_new_clusterID
      iterations <- iterations + 1
    }
  }
  
  # Plot the final cluster after iteration stops
  plot(data_with_cluster[,1:2], col = data_with_cluster$clusterID)
  points(centroid.matrix[,1:2], pch = 20, cex = 2, col = 1:k)
  
  # when iterations are done, return clusters assigned and final centroid
  return(list("clusters" = km.clusters, "centroids" = centroid.matrix))
}

# call the K-Means function we just created
km_clusters <- k_means(cluster.data, k = 3, max.iter = 15)

# look into the object returned by our function
print(km_clusters)



# TASK 2b. Elbow Heuristics to determine best guess for value of K in kmeans().
# For this part, use built-in kmeans() function in R
# K-means is used on synthetic data for values of K ranging from 1 to 7
# total within sum of square for each one of them is saved

# initialize a vector to store total within ss for different values of K
within_ss <- numeric(7)


# Run k-means seven times each time changing value of K from 1 to 7
for(k in 1:7){
  # call built-in kmeans() function in R
  km.cl <- kmeans(cluster.data, centers = k)
  
  # save total within ss value for different values of k
  within_ss[k] <- km.cl$tot.withinss
}

# plot number of centers Vs. total within SS
plot(x = 1:7, y = within_ss, type='b', xlab = "number of centers K", ylab = "total within SS" )

# Based on the plot, write a comment describing what value of K would you choose.
# For the elbow effect: As k increases the average within-cluster variance decreases.
# Based on the plot, the best number of cluster (K) is 3.



###### Part 3: K-Medoids Clustering ######

# TASK 3a. Use pam() in "cluster" library for k-medoids clustering. Try with value of k = 3
# use manhattan metric for distance calculation.

library('cluster')
# perform clustering using pam()
kmed.cl <- pam(cluster.data, k = 3, metric = "manhattan")

# plot points
plot(cluster.data, col = kmed.cl$clustering, pch = as.character(kmed.cl$clustering), cex = 0.7, main="k-medoids")
# show medoids in the plot
points(kmed.cl$medoids, pch = c("1","2","3"), cex = 2, lwd=3)

# TASK 3b. Calculate Silhouette value for clustering for K ranging from 2 to 5.
# Use manhattan distance

# dataframe to hold Silhouette value
sil_info <- data.frame("K" = 2:5, "Sil" = numeric(4))

# repeat pam() for value of k from 2 to 5
# average silhouette width can be accessed as "silinfo$avg.width"

for(k in 2:5){
  kmed.cl <- pam(cluster.data, k = k, metric = "manhattan")
  sil_info$Sil[k-1] <- kmed.cl$silinfo$avg.width
}

print(sil_info)

# Based on the table above, what value of K gives the best result
# The silhouette width measures how well the unit i is clusterd, it can vary from -1 to 1.
# If the silhouette width is closed to 1 it means that the clustering process was appropriate.
# The value of K that gives the best result is 3. With K = 3 we have the highest value of average silhouette width.


###### Part 4: Hierarchical Clustering ######

# TASK 4a. Perform hierarchical clustering using built-in R function
# Create smaller dataset of only 20 observations randomly picked

set.seed(101)
rand.sample <- sample(1:nrow(cluster.data), 20)
small.dataset <- cluster.data[rand.sample,]
rownames(small.dataset) <- 1:20

# Perform clustering on "small.dataset"
# Calculate distance matrix using dist() function in R

distance_matrix <-  dist(small.dataset, method = "euclidean")

# Call a built-in R function to perform hierarchical clustering
hierarchial.clust <- hclust(d = distance_matrix, method = "average")
plot(hierarchial.clust, hang = 0.1, main = "Hierarchical Cluster", cex = 1)


# TASK 4b. Cut the dendrogram to get three clusters
#
clusters <- cutree(hierarchial.clust, 3)
plot(small.dataset, col = clusters, pch = as.character(clusters), cex = 0.7)


########################################### END ##############################################
