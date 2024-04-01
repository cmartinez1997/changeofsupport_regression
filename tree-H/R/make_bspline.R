# X <- matrix(rnorm(10 * 5), 10, 5)
# colnames(X) <- letters[1:5]
library(splines)

make_pairwse_hadamard <- function(A, B) {
    return(A[,rep(seq_len(ncol(A)), each=ncol(B))] * as.vector(B))
}
make_bspline <- function(X, df = 4, interaction = FALSE) {
    n_vars <- ncol(X)
    B_list <- sapply(1:n_vars, function(i) bs(X[, i], df, intercept = FALSE), simplify = FALSE)
    B <- do.call(cbind, B_list)
    col_names <- c(sapply(1:n_vars, function(i) paste(colnames(X)[i], 1:df, sep="_")))
    colnames(B) <- col_names
    if (interaction & n_vars > 1) {
        spline_idx <- rep(1:n_vars, each = df)
        spline_int_idx <- rep(1:(n_vars * (n_vars - 1) / 2), each = df^2)
        B_int <- matrix(0, nrow(X), n_vars * (n_vars - 1) / 2 * df^2)
        idx <- 1
        # name permutations
        k_idx <- expand.grid(rep(1:df), rep(1:df))
        col_names_vec <- c()
        for (i in 1:n_vars) {
            if (i > 1) {
                for (j in 1:(i-1)) {
                    print(paste0("i = ", i, " and j = ", j))
                    # B_int[spline_idx == , ]
                    B_int[, spline_int_idx == idx] <- make_pairwse_hadamard(B[, spline_idx == i], B[, spline_idx == j])
                    idx <- idx + 1
                    col_names_vec <- c(col_names_vec, paste(col_names[spline_idx == i][k_idx[, 1]], col_names[spline_idx == j][k_idx[, 2]], sep=" x "))
                }
            }
        }
        colnames(B_int) <- col_names_vec
        B <- cbind(B, B_int)
    } else {
        # add in spline attributes
        attr(B, "degree") <- sapply(1:n_vars, function(i) attr(B_list[[i]], "degree"))
        attr(B, "knots") <- sapply(1:n_vars, function(i) attr(B_list[[i]], "knots"))
        attr(B, "Boundary.knots") <- sapply(1:n_vars, function(i) attr(B_list[[i]], "Boundary.knots"))
        attr(B, "intercept") <- sapply(1:n_vars, function(i) attr(B_list[[i]], "intercept"))
        attr(B, "class") <- sapply(1:n_vars, function(i) attr(B_list[[i]], "class"))
    }
    return(B)   
}


