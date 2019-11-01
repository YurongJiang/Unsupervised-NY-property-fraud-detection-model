load("pca_data_fixed.rda")
## get rid of Record
pca_data = as.data.frame(pca_data_fixed)[c(-2)]

## Transform NA into 0 in data
n1 = nrow(pca_data); n2 = ncol(pca_data);
for(i in c(1:n2)){
  pca_data[, i] = ifelse(is.na(pca_data[, i]), 0, pca_data[, i])
}

## Z-Scale
pca_data_bef_z = pca_data
n_bef_z = ncol(pca_data_bef_z)
for(i in 1:n_bef_z){
  stand_deviation_bef_z = sd(pca_data_bef_z[,i])
  mean_bef_z = mean(pca_data_bef_z[,i])
  pca_data_bef_z[,i] = (pca_data_bef_z[,i] - mean_bef_z) / stand_deviation_bef_z
}

## check the correlation (in case of correlation = 1 between different variables)
correlation = cor(pca_data_bef_z)
## save(correlation, file = "correlation.csv")
pca_data_input = pca_data_bef_z


## PCA
pca_result = prcomp(pca_data_input, center = F, scale = F)
## if we do not do z-scale manually, we use "center = T, scale = T" and get the same output
print(pca_result) ## standard deviations
plot(pca_result, xlab = "Principal Component")
sum(pca_result$sdev[1:13]^2)/ sum(pca_result$sdev^2)
## composition of each PC
pca_result$rotation


## compute standard deviation of each principal component
std_dev = pca_result$sdev
## compute variance
pc_var <- std_dev^2
## check variance of first 20 components
pc_var[1:20]
## proportion of variance explained
prop_var_ex <- pc_var/sum(pc_var)
prop_var_ex[1:20]

## proportion plot
plot(prop_var_ex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")
plot(prop_var_ex[1:20], xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")
## there is an odd at pc 13
## therefore, we should cut at 13 or 14, we cut at 13


## cumulative scree plot
plot(cumsum(prop_var_ex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")

## proportion of top 13 pcs
sum(prop_var_ex[1:13])



## Fraud Score based on Euclidean Distance
## top 13
n_top = 13
## calculate the transformed record metric
## now what we have are the projection value of orginal dataset on the new n_top PC directions.
## score of each record based on new pcs
pca_matrix = predict(pca_result, newdata = pca_data_input) 
## prcomp returns a list with class "prcomp" containing the following components:
## sdev, rotation, x, cenrter, scale
dim(pca_matrix)
## extract top 13 pcs in pca_matrix
pca_matrix_13 = as.data.frame(pca_matrix)[,c(1:n_top)]

## as.data.frame so as to falicitate calculation
pca_z = as.data.frame((pca_matrix_13))
## pca_z_ae stores the current pca_z value left for autoencoder
pca_z_ae = pca_z
## score = sum of projections on the top 13 directions
pca_z$Total_score = sqrt(rowSums(pca_z[, c(1:n_top)]^2))
pca_z$SumAbs = rowSums(abs(pca_z[, c(1:n_top)]))
MaxAbs = 1:nrow(pca_z)
for(i in 1:nrow(pca_z)){
  MaxAbs[i] = max(abs(pca_z[i, c(1:n_top)]))
}
pca_z_top = pca_z[,1:n_top]
pca_z_top = abs(pca_z_top)
pca_z_top$AbsMax <- apply(pca_z_top, 1, max)

pca_z$ID = 1:nrow(pca_z)
## descendingly reorder the data 
### index
sorted_index = order(pca_z$Total_score, decreasing = T) 
### reorder
pca_z_order = pca_z[sorted_index, ]
## check the top 10
head(pca_z_order,10)
## check the top 10000
pca_ID_10000 = pca_z_order$ID[1:10000]



## autocoder

library(h2o)
## input data
pca_z_ae
## set autoencoder
localH2O = h2o.init()
feature_names = names(pca_z_ae)
prostate.hex<-as.h2o(pca_z_ae, destination_frame="train.hex")
prostate.dl = h2o.deeplearning(x = feature_names, training_frame = prostate.hex,
                               autoencoder = TRUE,
                               reproducible = T,
                               seed = 1234,
                               hidden = c(5,5), epochs = 50)

# MSE of each record
prostate.anon = h2o.anomaly(prostate.dl, prostate.hex, per_feature=FALSE)
# head(prostate.anon)
head(prostate.anon)
err <- as.data.frame(prostate.anon)

ae_err = data.frame(pca_z_ae,err)
ae_err$ID = 1:nrow(ae_err)
## descendingly reorder the data 
### index
sorted_index_ae = order(ae_err$Reconstruction.MSE, decreasing = T) 
### reorder
ae_err_order = ae_err[sorted_index_ae, ]
## top ten
head(ae_err_order$ID,10)
## check the top 10000
ae_ID_10000 = ae_err_order$ID[1:10000]



## Comparison the overlapping between Euclidean and Autoencoder
similarity = match(ae_ID_10000, pca_ID_10000)

for(i in 1:10000){
  similarity[i] = ifelse(is.na(similarity[i]), 0, 1)
}
overlap_prop = sum(similarity[1:10000])/10000
## 76.07 %

## top ten
head(ae_err_order$ID,10)
head(pca_z_order$ID,10)
pca_data_fixed$RECORD[head(ae_err_order$ID,30)]
pca_data_fixed$RECORD[head(pca_z_order$ID,30)]

## visualize the fraud score distribution
library(ggplot2)
ggplot(data = ae_err_order, aes(x = Reconstruction.MSE)) + 
  geom_histogram(bins = 100, fill = "lightblue") + 
  scale_x_log10() +
  scale_y_sqrt() + 
  ggtitle("Distribution of MSE in Autoencoder") + 
  theme(panel.background = element_blank())

ggplot(data = pca_z_order, aes(x = Total_score)) + 
  geom_histogram(bins = 100, fill = "lightblue", position = position_dodge(5)) + 
  scale_x_log10() +
  scale_y_sqrt() + 
  ggtitle("Distribution of Euclidean Distance") + 
  theme(panel.background = element_blank())

ggplot(data = pca_z, aes(x = SumAbs)) + 
  geom_histogram(bins = 100, fill = "lightblue", position = position_dodge(5)) + 
  scale_x_log10() +
  scale_y_sqrt() + 
  ggtitle("Distribution of Sum of Absolute Value of PCs") + 
  theme(panel.background = element_blank())

ggplot(data = pca_z_top, aes(x = AbsMax)) + 
  geom_histogram(bins = 100, fill = "lightblue", position = position_dodge(5)) + 
  scale_x_log10() +
  scale_y_sqrt() + 
  ggtitle("Distribution of Maximum Absolute PC Value") + 
  theme(panel.background = element_blank())




theme_clean = function(base_size = 12) {
  require(grid)
  theme_grey(base_size) %+replace%
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.ticks.length = unit(0, "cm"),
      axis.ticks.margin = unit(0, "cm"),
      panel.margin = unit(0, "lines"),
      plot.margin = unit(c(0, 0, 0, 0), "lines"),
      complete = TRUE
    )
}



