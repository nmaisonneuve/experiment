Counter <- Object$clone()$do({
 init <- function() self$counter <- 0 
 count <- function() { 
    self$counter <- self$counter + 1 self$counter 
} 
}) 
counter_a <- Counter$clone() 
counter_a$count()




