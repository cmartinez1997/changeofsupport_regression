# make_polynomials

make_polynomials <- function(formula, dat) {
    model.matrix(formula, data = dat)    
}

# Example
N <- 10
dat <- data.frame(y = rnorm(N), x1 = rnorm(N), x2 = rnorm(N), x3 = rnorm(N))
# all interactions and quadratic terms
f <- y ~ (x1 + x2 + x3)^2 + I(x1^2) + I(x2^2) + I(x3^2)
X <- make_polynomials(f, dat)
head(X)

# all interactions -- Somewhat dangerous as it adds ALL columns of the data `dat`
f <- y ~ .^2
X <- make_polynomials(f, dat)
head(X)
# Only some interactions
f <- y ~ (x1 + x2)^2
X <- make_polynomials(f, dat)
head(X)
# Some interactions and all linear terms
f <- y ~ (x1 + x2)^2 + .
X <- make_polynomials(f, dat)
head(X)
