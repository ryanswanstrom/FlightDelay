mtcars
head(mtcars)
mtcars$mpg
mtcars[, 1]

mtcars[, 1] + 20

x <- 1:20
for (i in 1:20) {
    x[i] <- i^2
}
x
x <- 1:20
sapply(x, function(a) {a^2 })

