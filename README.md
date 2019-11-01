# Unsupervised-NY-property-fraud-detection-model
Applying fraud analysis in the NY Property Tax dataset with unsupervised approach


The Property Valuation and Assessment Data represents __NYC properties assessments for the purpose of calculating Property Tax__, Grant Eligible Properties Exemptions and/or Abatements. Data was collected and entered into the system by various City employees, like Property Assessors, Property Exemption specialists, ACRIS reporting, and Department of Building reporting.  

The data was created on September 2, 2011 and updated annually. The final assessment time of this dataset is 2011-11-01. Therefore, it covers various information of each property in New York City by November 1, 2011. There are __1070994 records__ and __32 fields__ in the dataset.   

Following is description of the variables we consider to be the most important. The complete __Data Quality Report__ can be found in appendix.   


### __Project Outline__ 

The original dataset consists of information including sizes, building classes, values, tax classes owner of about 1 million New York properties. The general process of analysis step includes:  

__1.     Data cleaning and missing data filing.__ We proposed the dataset to optimize the results of the analysis. we filed missing values by methods like using mean of the group the missing value belongs to or mode of properties which close to it in geographical location.  

__2.     Building expert variables and standardizing.__ we built special variables that look for fraud model and scaled them before put them into machine learning models.    

__3.     Dimensionality reduction through the PCA process.__ We only keep the main PCs and Z scale the data field again. 

__4.     Applying fraud algorithm, calculating fraud score identifying potential fraud.__ We combined the Z scores as the first fraud score and used the reconstruction error in the process of autoencoder as the second score. Final fraud score is a combination of these two scores.     

With the highest Cumulative average of __Heuristic Fraud scores__ and __Autoencoder Fraud scores__, we found the top 10 records which look anomalous and could be classified as underlying real estate frauds.    

__Detailed examination on top 10 high scores__ shows that abnormalities mainly due to three reasons:    
__1.     Accidentally input wrong data__   
__2.     Falsely report property value to cheat banks__  
__3.     Tax avoidance__   
