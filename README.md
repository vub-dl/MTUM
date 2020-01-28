
### Overview multitreatment uplift modeling (MTUM)

Uplift modeling is an instrument to estimate the change in outcome due to a treatment at the individual entity level. Uplift models assist decision-makers in optimally allocating scarce resources. This allows the selection of the subset of entities for which the effect of a treatment will be largest and, as such, the maximization of the overall returns. 

Single treatment studies have dominated the uplift modeling literature, and applications where more than one treatment alternative is utilized are rarely considered. Therefore, this repository shares with the community scripts of multitreatment uplift modeling techniques and evaluation approaches in order to estimulate the research on multitreatment uplift modeling and make reproducible and verifiable the results of the article [A survey and benchmarking study of multitreatment uplift modeling](https://link.springer.com/article/10.1007/s10618-019-00670-y).

Our study proposes two novel techniques: the naive uplift approach and the multitreatment modified outcome approach. Moreover, a benchmarking experiment is performed to contrast the performances of different multitreatment uplift modeling techniques across eight data sets from various domains. We verify and, if needed, correct the imbalance among the pretreatment characteristics of the treatment groups by means of optimal propensity score matching, which ensures a correct interpretation of the estimated uplift. Conventional and recently proposed evaluation metrics are adapted to the multitreatment scenario to assess performance. 

### Usage 

The methods implemented in the analysis considered the following packages:

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
uplift_matching <- ps_matching(dataset,"treatment_column","name_treatment_1","name_treatment_2","name_control", vector_features)
new_dataset <- uplift_matching$df
```
Create the training and test folds by applying stratified cross-validation:

```r
folds <- stratified.crossvalidation(new_dataset,"name_control","name_treatment_1","name_treatment_2","name_outcome",10)
training_df <- list()
test_df <- list()
training_indices <- list()
test_indices <- list()
for (i in 1:10){
  training_indices[[i]] <- unlist(folds[-i])
  test_indices[[i]] <- folds[[i]]
  training_df[[i]] <- new_dataset[training_indices[[i],]
  test_indices[[i]] <- new_dataset[test_indices[[i],]
}
```
Training multitreatment uplift modeling:

```r
model <- list()
for (i in 1:10){
  model[[i]] <-model_mtum(training_df[[i]],"name_treatment_1","name_treatment_2","name_outcome","YES","NO",vector_features,"name_model")
```
Model predictions:

```r
precictions <- list()
for (i in 1:10){
predictions[[i]] <- predict_mtum("approach_name",test_df[[i]],"name_outcome",vector_features,"YES","NO","name_treatment_1","name_treatment_2",model[[i]])
```
Evaluate model performance:

```r
predicted_uplift <- list()
performance <- list()
er <- list()
res_qini <- list()

for (i in 1:10){
predicted_uplift[[i]] <- uplift_score(predictions[[i]],"model_name",test_df[[i]],"name_control","treatment_column","name_treatment_1","name_treatment_2")

performance[[i]] <- performance_table(predictions[[i]], test_df[[i]],predicted_uplift[[i]],"treatment_column","name_outcome","name_control","name_treatment_1","name_treatment_2")

er[[i]] <- expected_response(test_df[[i]],predictions[[i]],predicted_uplift[[i]],"treatment_column","name_treatment_1","name_treatment_2","name_control")

res_qini[[i]] <- qini(performance[[i]], 1)
}
```
