
left <- data.frame(col1 = c(letters) , col2 = c(26:1))
right <- data.frame(col1 = c(letters[1:10], "A", "B") , col3 = c(50:61))

library(dplyr)
left_join(left, right, by = "col1")

right_join(left, right, by = "col1")

inner_join(left, right, by = "col1")

full_join(left, right, by = "col1")

left
arrange(left, desc(col1))   # sorts dataframe based on col1 in descending order
sort(left$col1, decreasing = T)   #sorts vector col1 in decreasing order
arrange(left, col2)         # sorts dataframe based on col2 in ascending order
