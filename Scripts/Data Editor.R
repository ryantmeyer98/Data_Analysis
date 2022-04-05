
# INSTALL REQUIRED PACKAGES ----
install.packages("DataEditR")
install.packages("devtools")

# I DONT KNOW WHAT THIS IS BUT YOU NEED IT ----
library(devtools)
devtools::install_github("DillonHammill/rhandsontable")

# LOAD DATA EDITOR ----
library(DataEditR)

# EXAMPLE FOR LOADING UP DATA EDITOR ON AN EXISTING DATA SET
mtcars_new <- data_edit(mtcars,
                        save_as = "mtcars_new.csv")

# NOW WHAT IF WE WANT TO MAKE OUR OWN DATA FRAME FROM SCRATCH?

# these create a framework to make a data frame around to edit
c <- c(1, 2, 3) 
r <- c(1, 2, 3) 

# puts c an r into a data frame
test.df <- data.frame(c, r)

# runs data editor to open the data frame
test.df <- data_edit(test.df)
