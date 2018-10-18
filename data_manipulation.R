####
#### By Elim Thompson (10/14/2018)
####
#### This R script follows the Kaggle Lesson -
#### Machine Learning in R Level 2: Manipulating
#### Data with the Tidyverse
####
#### https://www.kaggle.com/rtatman/manipulating-data-with-the-tidyverse
####
################################################

################################################
#### set up
################################################
### load tidyverse for useful functions
library (tidyverse)
### read a txt data file
farmData <- read_csv ("/home/elims/atom/learnR/kaggle_BDT/data/mussel-watch/histopaths.csv")
### print sorted column names
print (sort (names (farmData)))
### show dimension (# rows and # columns) in the data set
print (dim (farmData))

################################################
#### Piping `A %>% B`:
#### output of A is fed as an input of B
####
#### Piping allows a clear logic in the code
#### which is easier for debuggin
################################################
### 1. create vector > mean > product
##     without piping
print (prod (mean (c(1,2,5,5,10))))
##     with piping
print (c(1,2,5,5,10) %>% mean () %>% prod ())

### 2. piping does not change the initial input
##     define variable
myvar <- "some text"
##     save a copy
myvar_before <- myvar
myvar_pipe   <- myvar
myvar_nopipe <- myvar
##     do things without piping
#      a. split string to characters; return a list
print (strsplit (myvar_nopipe, split = ""))
#      b. convert list to a vector
print (unlist (strsplit (myvar_nopipe, split = "")))
#      c. sort the vector
print (sort (unlist (strsplit (myvar_nopipe, split = ""))))
##     do the same thing with piping
print (myvar_pipe %>%
       strsplit (split="") %>%
       unlist () %>%
       sort ())
##     check if the myvar's are the same
print (identical (myvar, myvar_pipe))
print (identical (myvar, myvar_nopipe))
print (identical (myvar, myvar_before))

### 3. caveat: piping passes the output of A as the
###            *first* argument to B
##     lets say round ()
print (piped <- 3.14152 %>%
          round (1.5690))
##     the above output is the same as this
print (round (3.14152, 1.5690))
##     but not this
print (round (1.5690, 3.14152))

### 4, apply piping to the dataset to get a list
###    of all column names in alphabetic order
sorted_column_names <- farmData %>%
                names () %>%
                sort ()
print (sorted_column_names)
print (farmData$dermo)

################################################
#### Manipulate a specific column via
#### `select (not_str)`
####
#### select () comes with many helper functions:
####   - starts_with ("str")
####   - ends_with ("str")
####   - contains ("str")
####   - one_of (vector of str)
################################################
### 1. select the ceroid column and
###    print the first six rows
print (farmData %>%
          select (abnormality_description) %>%
          head (3))

### 2. remove a column via `-` sign
print (farmData %>%
          select (-abnormality_description) %>%
          head ())

### 3. select columns with names starting with
###    a specific prefix
print (farmData %>%
          select (starts_with ("cestode")) %>%
          head ())

### 4. remove columns with names starting with
###    a specific prefix
print (farmData %>%
          select (-starts_with ("cestode")) %>%
          head ())

################################################
#### Reach specific rows via `filter ()`
#### `filter ()` returns a subset of data that
#### fulfill certain criteria.
####
#### common logic expression:
####   - < less than
####   - <= less than or equal to
####   - != not equal to
####   - | or
####   - & and
################################################
### 1. pick dermo == 'L'
Ldermo <- farmData %>%
            filter (dermo == 'L')
print (dim (Ldermo))

### 2. pick rows with wet_weight > 5
wwgt5 <- farmData %>%
            filter (wet_weight > 5)
print (dim (wwgt5))

### 3. see if any rows that satisfy both criteria
Ldermo_wwgt5 <- farmData %>%
                  filter (dermo=="L" & wet_weight>5)
print (dim (Ldermo_wwgt5))

### 4. filter all the rows where
###    abnormality is greater than 3
abgt3 <- farmData %>%
            filter (abnormality > 3)
print (dim (abgt3))

### 5. filter all the rows where
###    coastal_ecological_area is "Lake Michigan"
cearea <- farmData %>%
            filter (coastal_ecological_area == "Lake Mechigan")
print (dim (cearea))

### 6. filter all the rows where
###    sex is "Male" and state_name is "Mississippi"
male_MI <- farmData %>%
              filter (sex == "Male" & state_name == "Mississippi")
print (dim (male_MI))

################################################
#### Add new column via `mutate ()`
####
#### This is often used to create dummy
#### variables for columns in string.
################################################
### 1. create a new column that states TRUE if
###    dermo has "L", excluding L+ / L- / etc
##     add new columns : TRUE / FALSE
farmData <- farmData %>%
                mutate (Ldermo = (dermo=="L"))
##     summaries the new column
print (summary (farmData$Ldermo))

### 2. add a new variable called large_mussels
###    that is TRUE if a mussel is over 10 oz.
farmData <- farmData %>% mutate (large_mussels = (wet_weight > 10))

### 3. add a new variable parasites that is TRUE
###    if a mussel has more than 1 unidentified_organism
###    in it and a wet_weight of less than 0.5 oz
farmData <- farmData %>% mutate (parasites = (unidentified_organism > 1 & wet_weight < 0.5))
print (farmData %>% summary(parasites))
?na.omit

### More fancy stuff can be done with mutate () !!

################################################
#### Arrange a column via `arrange ()`
####
#### This sort the entire data frame based on
#### the arranged column. This is helpful when
#### one is looking for a trend / correlation
#### between variables.
################################################
### 1. arrange wet_weight
print (farmData %>% arrange (wet_weight) %>% head ())
###    just look at the large_mussels
print (farmData %>% select (wet_weight, large_mussels) %>% arrange (wet_weight) %>% head ())

### 2. descending order
print (farmData %>% arrange (desc (wet_weight)) %>% head ())
###    just look at the large_mussels
print (farmData %>% select (wet_weight, large_mussels) %>% arrange (desc (wet_weight)) %>% head ())

### 3. sort the musselData data frame so that the
###    longest mussels are first
print (farmData %>% arrange (desc (length)) %>% select (length, wet_weight) %>% head ())

################################################
#### Summarize a variable via `summarize ()`
####
#### It takes a column name and spit a new
#### data frame with the statistic info of
#### the values from that column.
################################################
### 1. get the mean value of wet_weight / length
farmData %>% summarize (meanlength=mean (length),
                        meanweight=mean (na.omit (wet_weight)))

### 2. create a single data frame with the mean
###    and median length
farmData %>% summarize (meanlength=mean (length), medianlength=median (length))

################################################
#### Analyze a group via `group_by ()`
####
#### It takes a column name and spit a new
#### data frame with the statistic info of
#### the values from that column.
################################################
