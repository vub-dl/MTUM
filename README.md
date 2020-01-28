
### Overview multitreatment uplift modeling (MTUM)

Mutitreatment uplift modeling techniques and evaluation approaches 

Uplift modeling is an instrument to estimate the change in outcome due to a treatment at the individual entity level. Uplift models assist decision-makers in optimally allocating scarce resources. This allows the selection of the subset of entities for which the effect of a treatment will be largest and, as such, the maximization of the overall returns. 

Single treatment studies have dominated the uplift modeling literature, and applications where more than one treatment alternative is utilized are rarely considered. Therefore, this repository shares with the community scripts of multitreatment uplift modeling techniques and evaluation approaches in order to estimulate the research on multitreatment uplift modeling and make reproducible and verifiable the results of the article [A survey and benchmarking study of multitreatment uplift modeling](https://link.springer.com/article/10.1007/s10618-019-00670-y).

Our study proposes two novel techniques: the naive uplift approach and the multitreatment modified outcome approach. Moreover, a benchmarking experiment is performed to contrast the performances of different multitreatment uplift modeling techniques across eight data sets from various domains. We verify and, if needed, correct the imbalance among the pretreatment characteristics of the treatment groups by means of optimal propensity score matching, which ensures a correct interpretation of the estimated uplift. Conventional and recently proposed evaluation metrics are adapted to the multitreatment scenario to assess performance. 

Load the packages:

```r
packages <- list("caret","splitstackshape","scales","corrplot","AppliedPredictiveModeling","ggplot2","uplift","plyr","ggpubr","moments","stringr","sas7bdat","randomForest","nnet","twang","RItools","RItools","tableone","MatchIt","car","reshape2","ggthemes","rowr")
lapply(packages, require, character.only = TRUE)
```
Load the scripts:

```r
scripts <- list("bias_correct.R","stratified.R","models.R","prediction.R","performance.R")
lapply(scripts, source)
```
Apply matching to reduce the correct selection bias:

```r
Uplift_matching <- ps_matching(dataset,"treatment_column","name_treatment_1","name_treatment_2","name_control", vector_features)
```
Create the training and test folds by applying stratified cross-validation:

```r
folds <- stratified.crossvalidation(dataset,"name_control","name_treatment_1","name_treatment_2","name_outcome",10)
```
Training multitreatment uplift modeling:

```r
model <- model_mtum(training_dataset,"name_treatment_1","name_treatment_2","name_outcome","YES","NO",vector_features,"name_model")
```
Model predictions:

```r
predictions <- predict_mtum("approach_name",test_dataset,"name_outcome",vector_features,"YES","NO","name_treatment_1","name_treatment_2",model)
```
Evaluate model performance:

```r
predicted_uplift <- uplift_score(predictions,"model_name",test_dataset,"name_control","treatment_column","name_treatment_1","name_treatment_2")

performance <- performance_table(predictions, test_dataset,predicted_uplift,"treatment_column","name_outcome","name_control","name_treatment_1","name_treatment_2")

er <- expected_response(test_dataset,predictions,predicted_uplift,"treatment_column","name_treatment_1","name_treatment_2","name_control")
```
Compute the qini metric:

```r
res_qini <- qini(performance, 1)
```





