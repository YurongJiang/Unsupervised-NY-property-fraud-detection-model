load("mydata.rda")
data = datacsv1

# Divide orginal data into two
## Transform NA into 0 in data
for(i in c(9:14,19)){
  data[, i] = ifelse(is.na(data[, i]), 0, data[, i])
}
data_index = data[c(9:14,19)]

## Score the quality of each information (row)
data_index$Score = 0
for(i in 1:7){
  data_index[, 8] = data_index[, 8] + ifelse(data_index[, i] ==0, 1, 0)
}

## Clear the character columns of data
data = data[, -c(2,5,6,18,20,28:30)]

## Partition
data_lowscore = subset(data, data_index[, 8] <= 2.5); nrow(data_lowscore)
data_highscore = subset(data, data_index[, 8] > 2.5); nrow(data_highscore)

## Save two datasets
write.csv (data_lowscore, "data_lowscore.csv", row.names=FALSE)
write.csv (data_highscore, "data_highscore.csv", row.names=FALSE)


# Visualize the distribution
## Distribution of Score
library(ggplot2)
ggplot(data = data_index, aes(x = as.factor(Score))) + 
  geom_bar() + 
  ggtitle("Distribution of Score")













