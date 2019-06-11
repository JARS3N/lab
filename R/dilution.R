#' dilution
#'
#' @param c1 
#' @param v1 
#' @param c2 
#' @param v2 
#'
#' @return vector
#' @export 
#'
#' @examples
dilution <-
function(c1 = NULL,
                     v1 = NULL,
                     c2 = NULL,
                     v2 = NULL
) {
    IN <- list(
        "c1" = c1,
        "v1" = v1,
        "c2" = c2,
        "v2" = v2
    )
    check_null <- lapply(IN, is.numeric)
    check <- sum(!unlist(check_null))
    if (check != 1) {
        message("3 numeric inputs of c1,v1,c2,v2 required")
        return(NULL)
    }
    missing <- names(IN[!unlist(check_null)])
    
   out<- list("c1" = c2 * v2 * (v1 ^ -1),
         "v1" = c2 * v2 * (c1 ^ -1),
         "c2" = c1 * v1 * (v2 ^ -1),
         "v2" = c1 * v1 * (c2 ^ -1))[[missing]]
    cat(paste0(missing,": "))
    out
}
