# =============================================================================
# Lab 1: Introduction to R
#
# In this lab you will work through Section 2.3 of the course
# textbook, An Introduction to Statistical Learning.
# The R code from Section 2.3 is given below.
# =============================================================================

# -----------------------------------------------------------------------------
# 1. Basic Commands
# -----------------------------------------------------------------------------
x <- c(1, 3, 2, 5)
x
x <- c(1, 6, 2)
x
y <- c(1, 4, 3)
length(x)
length(y)
x + y
ls()
rm(x, y)
ls()
rm(list = ls())
?matrix
x <- matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2)
x
x <- matrix(c(1, 2, 3, 4), 2, 2)
matrix(c(1, 2, 3, 4), 2, 2, byrow = TRUE)
sqrt(x)
x^2

# ---------- Correlation Coefficient ----------
# sum((x - mean(x)) * (y - mean(y))) / length(x) / sd(x) / sd(y)
x <- rnorm(10)
y <- x + rnorm(10, mean = 50, sd = .1)
cor(x, y)

# ---------- MSE ----------
y <- c(1, 2, 2.5, 3, 4) # measured values
yhat <- c(1.1, 1.9, 2, 2.8, 4.2) # predicted values
MSE <- mean((y - yhat)^2)
MSE
MSE <- (1 / length(y)) * sum((y - yhat)^2)
MSE

# -----------------------------------------------------------------------------
# Graphics
# -----------------------------------------------------------------------------

# ---------- plot ----------
x <- rnorm(100)
y <- rnorm(100)
plot(x, y)
plot(x, y,
  xlab = "this is the x-axis",
  ylab = "this is the y-axis",
  main = "Plot of X vs Y",
  col = "red",
  pch = "o",
)

# ---------- contour diagram ----------
x <- seq(1, 10)
x
x <- 1:10
x
x <- seq(-pi, pi, length = 50)
y <- x
f <- outer(x, y, function(x, y) cos(y) / (1 + x^2))
contour(x, y, f)
contour(x, y, f, nlevels = 50, add = TRUE) # 50 contour levels, as an add-on

# ---------- image ----------
# same way as `contour`, except that it produces a color-coded plot whose
# colors depend on the z value
image(x, y, f)

# ---------- three-dimensional plot ----------
# theta gives the azimuthal direction and phi the colatitude
persp(x, y, f)
persp(x, y, f, theta = 30)
persp(x, y, f, phi = 40)
persp(x, y, f, theta = 30, phi = 20)
persp(x, y, f, theta = 35, phi = 20)
persp(x, y, f, theta = 40, phi = 20)


# -----------------------------------------------------------------------------
# Indexing Data
# -----------------------------------------------------------------------------
A <- matrix(1:16, 4, 4)
A
A[2, 3]
A[c(1, 3), c(2, 4)]
A[1:3, 2:4]
A[1:2, ]
A[, 1:2]
A[1, ]
A[-c(1, 3), ]
A[-c(1, 3), -c(1, 3, 4)]
dim(A)

# -----------------------------------------------------------------------------
# Loading Data
# -----------------------------------------------------------------------------
import::from(magrittr, "%>%", "%$%", .into = "operators")

Auto <- read.csv("data/Auto.csv", header = T, na.strings = "?")

Auto %>% dim()
Auto %>% names()
Auto %>% View()
Auto %>%
  summary() %>%
  View()

Auto %>%
  dplyr::select("mpg":"horsepower", "name") %>%
  dplyr::filter(cylinders <= 3) %>%
  View()

Auto <- na.omit(Auto)
dim(Auto)
names(Auto)

# -----------------------------------------------------------------------------
# Additional Graphical and Numerical Summaries
# -----------------------------------------------------------------------------
plot(cylinders, mpg) # do NOT like this; attach not recommended
# attach(Auto) # try not to use
plot(Auto$cylinders, Auto$mpg) # good
Auto %$% plot(cylinders, mpg) # better still
Auto %$% plot(as.factor(cylinders), mpg)

# number of cylinders
Auto$cylinders
cylinders <- as.factor(Auto$cylinders)
mpg <- Auto$mpg

plot(cylinders, mpg, col = "red")
# varwidth: box proportionate to the sample size
plot(cylinders, mpg, col = "red", varwidth = T)
plot(cylinders, mpg, col = "red", varwidth = T, horizontal = T)
plot(cylinders, mpg,
  col = "red", varwidth = T, horizontal = F,
  xlab = "Cylinders",
  ylab = "MPG"
)
hist(mpg)
hist(Auto$cylinders) # do NOT use factors
# sturges vs. scott
hist(mpg, col = "blue", breaks = "Sturges")
pairs(~ mpg + displacement + horsepower + weight + acceleration, Auto)
plot(Auto$horsepower, Auto$mpg)
identify(Auto$horsepower, Auto$mpg, Auto$name)
summary(Auto)
summary(Auto$mpg)
