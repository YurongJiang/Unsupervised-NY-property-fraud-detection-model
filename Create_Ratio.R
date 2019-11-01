## Create new ratio variables
load("pca_data_fixed.rda")
data = pca_data_fixed
data 

## locate the three basic feature
featureloc = match(c("FULLVAL", "AVLAND", "AVTOT"), names(data))
featureloc = c(featureloc, featureloc)

## size of current data
ndata = ncol(data)

## variable FULLVAL, AVLAND, AVTOT
## A, B, C

## ratio1(A/B, B/C, C/A)
ndata1 = ncol(data)
for(i in 1:3){
  data[, ndata1 + i] = data[, featureloc[i%%6]] / data[, featureloc[(i+1)%%6]]
}

## ratio2 = inverse(ratio1) = (B/A, C/B, A/C)
ndata2 = ncol(data)
for(i in 1:3){
  data[, ndata2+i] = 1 / data[, ndata2-3+i]
}

## ratio3 = A/(B+C), ## clear-- B/(C+A), C/(A+B)
ndata3 = ncol(data)
for(i in 1:3){
  data[, ndata3+i] = data[, featureloc[i%%6]] / (data[, featureloc[(i+1)%%6]] + data[, featureloc[(i+2)%%6]])
}

## ratio4 = A^2/(B*C), ## clear-- B^2/(C*A), C^2/(A*B)
ndata4 = ncol(data)
for(i in 1:3){
  data[, ndata4+i] = (data[, featureloc[i%%6]])^2 / (data[, featureloc[(i+1)%%6]] * data[, featureloc[(i+2)%%6]])
}

## ratio5 = B*C/(A^2), ## clear-- C*A/(B^2), A*B/(C^2),
ndata5 = ncol(data)
for(i in 1:3){
  data[, ndata5+i] = 1 / data[, ndata5-3+i]
}

## ratio6 = sqrt(B*C/(A^2)),## clear-- sqrt(C*A/(B^2)), sqrt(A*B/(C^2))
ndata6 = ncol(data)
for(i in 1:3){
  data[, ndata6+i] = sqrt(data[, ndata6-3+i])
}

## ratio7 = (A/B)^2, (B/A)^2, (C/A)^2
ndata7 = ncol(data)
for(i in 1:3){
  data[, ndata7+i] = (data[, featureloc[i%%6]] / data[, featureloc[(i+1)%%6]])^2
}



## locate the three basic feature
variableloc = match(c("LTFRONT", "LTDEPTH", "STORIES", "FULLVAL", "AVLAND", "AVTOT", "EXLAND", "EXTOT", "EXCD1"), names(data))
variableloc = c(variableloc, variableloc)

## sqrt each variable (9)
coldata1 = ncol(data)
for(i in 1:9){
  data[, coldata1 + i] = sqrt(data[, variableloc[i]])
}


## ln(x+1) each variable (9)
coldata2 = ncol(data)
for(i in 1:9){
  data[, coldata2 + i] = log(1 + data[, variableloc[i]])
}

## sqrt(LTFRONT*LTDEPTH) so as sqrt(STORIES*LTFRONT), sqrt(LTDEPTH*STORIES)
coldata3 = ncol(data)
featureloc1 = match(c("LTFRONT", "LTDEPTH", "STORIES"), names(data))
featureloc1 = c(featureloc1, featureloc1)
for(i in 1:3){
  data[, coldata3 + i] = sqrt(data[, featureloc1[i%%6]] * data[, featureloc1[(i+1)%%6]])
}

pca_data = data


# Divide orginal data into two
## Transform NA into 0 in data
n1 = nrow(pca_data); n2 = ncol(pca_data);

for(i in c(1:n2)){
  pca_data[, i] = ifelse(is.na(pca_data[, i]), 0, pca_data[, i])
}

# delete certain variables
deleteloc = match(c("newBLOCK", "rest", "RECORD", "LOT", "BLDGCL", "TAXCLASS", "ZIP"), names(pca_data))
pca_data = pca_data[-c(deleteloc)]

save(pca_data, file = "pca_data_final.rda")






