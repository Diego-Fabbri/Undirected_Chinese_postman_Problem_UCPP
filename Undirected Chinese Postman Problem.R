#Set your own working directory
setwd("C:/Users/diego/Documents/R/Projects/GitHub_Projects/Optimization/Undirected Chinese Postman Problem")

# Import lpSolve package
library(lpSolve)

#Import required packages (ompr)
library(dplyr)
library(ROI)
library(ROI.plugin.symphony)
library(ompr)
library(ompr.roi)
library(ggplot2)

#Set costs
c <-matrix(c(100000,1,5,4,
             1,100000,2,6,
             5,2,100000,3,
             4,6,3,100000), nrow = 4, byrow = TRUE)

#Set problem size
n <- ncol(c)

#Build Model
Model <- MIPModel() %>%
  add_variable(x[i,j], i = 1:n, j = 1:n, type = "integer", lb = 0) %>% #define variables
  set_bounds(x[i,i], i = 1:n, lb = 0, ub = 0) %>%
  set_objective(sum_expr(c[i, j] * x[i, j], i = 1:n, j = 1:n), "min") %>% #define objective
  add_constraint(sum_expr(x[i, j] - x[j, i], j = 1:n) == 0, i = 1:n) %>% #define constraints
  add_constraint(x[i, j] + x[j, i] >= 1,i = 1:n, j = 1:n, i<j) %>%
  solve_model(with_ROI(solver = "symphony", verbosity = 1))

#Model summary
##Status
print(paste("Model status is:", Model$status))

##Objective Function
print(paste("Objective value:", objective_value(Model)))

for (a in 1:n) {
  for (b in 1:n) {
    tmp_x <- get_solution(Model, x[i, j]) %>%
      filter(variable == "x", i == a, j == b) %>%
      select(value)
    
    
    if (tmp_x != 0) {
      print(paste("--->x[", a, ",", b , "] =", tmp_x))
    }
  }
}




