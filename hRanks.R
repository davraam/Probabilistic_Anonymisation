#' 
#' @title hRanks
#'
#' @description The function calculates a re-identification risk measure of anonymised data
#' 
#' @details The function calculates a re-identification risk measure, the h-metric, between two input
#' dataframes; one including the true data and the second including the anonymised data. This disclosure 
#' risk measure was proposed by Goldstein and Shlomo (2017). Initially (step 1), the algorithm calculates
#' the Euclidean distances between each row in the true dataset and all rows in the noisy dataset (i.e. a
#' pair-wise comparison that ultimately assesses all possible pairs). It then (step 2), ranks the distances
#' in order to determine how close each individual’s record is to every record in the anonymised dataset
#' (i.e. a 1 to n comparison where n is the total number of individual-levels), and identifies the position
#' of the closest record (i.e. the record which corresponds to rank equal to one). We use the standard 
#' competition ranking method (usually notated as “1224” ranking), which is performed by the R function 
#' rank with the argument ties.method=’min’. In step 3, the algorithm generates a duplicate copy of the
#' true dataset and computes the Euclidean distances between each row in the true dataset with all rows
#' in the copy of the true dataset and ranks them in order of distance (i.e. a 1 to n comparison similar
#' to step 2). In step 4, the algorithm identifies the ranks of the distances calculated in step 3 at the
#' locations specified in step 2. Finally (step 5), the algorithm calculates the difference (h-rank index)
#' between the ranks located in step 4 and step 2 and returns a vector with those differences (note the all
#' the ranks located at step 2 are equal to one as the method searches for the closest noisy records to each
#' true record). If h=0 for any one record that an attacker has available (and belongs to the dataset) then
#' this implies that the noisy record identified by the attacker as the closest one in terms of the distance
#' metric, is in fact the true one.
#'
#' @param true.data, is the dataframe that includes the original data.
#'
#' @param noisy.data, is the dataframe that includes the anonymised data. The noisy.data dataframe has
#' the same dimensions with the true.data dataframe.
#'
#' @return The function returns a vactor with h-indices.
#' 
#' @author Avraam D.
#' @export
#'
hRanks <- function(true.data=NULL, noisy.data=NULL){

    # Convert the input dataframes to matrices
	true.data <- as.matrix(true.data)
	noisy.data <- as.matrix(noisy.data)
    
    # Check if the two datasets have the same dimensions. If they don't have the same
    # dimensions, the function returns an error message.
    n1 <- dim(true.data)
    n2 <- dim(noisy.data)
    if (n1[1]==n2[1] & n1[2]==n2[2]){
      n <- n1[1]
    }else{
      stop("The two input datasets have different dimensions", call.=FALSE)
    }
    	
    # Calculate distances Di
    Di <- flexclust::dist2(true.data, noisy.data)
	
    # Find the ranks of Di (row-wise)
    Ri <- matrixStats::rowRanks(Di, ties.method='min')
    
    # Find the positions in matrix Di with ranks equal to 1
    I <- NA
    for (i in 1:n){
      I[i] <- which(Ri[i,]==1)
    }     
    
    # Calculate distances Dk
    Dk <- flexclust::dist2(true.data, true.data)
	
    # Find the ranks of Dk (row-wise)
    Rk <- matrixStats::rowRanks(Dk, ties.method='min')
    
    # Calculate the h-ranks
    h <- NA
    for (i in 1:n){
      h[i] <- Rk[i,I[i]]-1
    }
    
    # Return the results
    return(h=h)
	
}
