# Syntax of ifelse() function:
  # ifelse(test_expression, x, y)

a = c(0,3,2,9)
ifelse(a %% 2 == 0,"even","odd")

#Average task
# Syntax is ave(x, …, FUN = mean)
# Arguments
# x      is a numeric.
# …      is Grouping variables, typically factors, all of the same length as x.
#FUN     is Function to apply for each factor level combination.
#Value will be A numeric vector, say y of length length(x). If … is g1, g2, e.g., y[i] is equal to FUN(x[j], for all j with g1[j] == g1[i] and g2[j] == g2[i])
#example
ave(1:3)  # no grouping -> grand mean

# mean

#mean(x, …)
## S3 method for default
#mean(x, trim = 0, na.rm = FALSE, …)


#x        Is An R object. Currently there are methods for numeric/logical vectors and date, date-time and time interval objects. Complex vectors are allowed for trim = 0, only.

#trim     the fraction (0 to 0.5) of observations to be trimmed from each end of x before the mean is computed. 
#         Values of trim outside that range are taken as the nearest endpoint.

#na.rm    a logical value indicating whether NA values should be stripped before the computation proceeds.
#…        further arguments passed to or from other methods.

#Value
#If trim is zero (the default), the arithmetic mean of the values in x is computed, as a numeric or complex vector of length one.  
#If x is not logical (coerced to numeric), numeric (including integer) or complex, NA_real_ is returned, with a warning.

#If trim is non-zero, a symmetrically trimmed mean is computed with a fraction of trim observations deleted from each end 
#before the mean is computed.
# NOT RUN {
x <- c(0:10, 50)
print(x)
xm <- mean(x)
print(xm)
xm2 <-mean(x, trim = 0.10)
print(xm2)
c(xm, mean(x, trim = 0.10))  #creating a vector
# }

#defining  x = 1 2 3
x <- 1:3
mean(x)
#introducing missing value
x[1] <- NA
# mean = NA
mean(x)

# floor(x) rounds to the nearest integer that’s smaller than x. So floor(123.45) becomes 123 and floor(-123.45) becomes –124.

# ceiling(x) rounds to the nearest integer that’s larger than x. This means ceiling (123.45) becomes 124 and ceiling(-123.45) becomes –123.

# trunc(x) rounds to the nearest integer in the direction of 0. So trunc(123.65) becomes 123 and trunc(-123.65) becomes –123.
