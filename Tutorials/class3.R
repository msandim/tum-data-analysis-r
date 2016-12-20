# 1.1
tmp_tidy_table <- "1_colname,2_colname,3_colname
3,4,5
a,b,c"

read.csv(text = tmp_tidy_table, check.names = FALSE)

# 1.2
tmp_messy_table <- "# This line is just useless info
1_colname,2_colname,3_colname
3,4,5
a,b,c"

#### fazer isto depois

# 2.1


# 3.1
library(XML)
doc <- xmlTreeParse("extdata/plant_catalog.xml", useInternal = TRUE)
root <- xmlRoot(doc)
unique