# Report PROBLEM 2

## Rdata File

Hwk3_1.RData file has ~200mb and is uploaded to Google Drive. Should be downloaded and placed in a right directory before starting the code. 

### Hwk3_1.RData - https://drive.google.com/file/d/1-LmautyMAaL7uB9fJCdWImkZvOFPboOi/view?usp=sharing

After loading the file the script is ready to run. RData file has all time consuming objects ready and stored.

## Book Crossing Dataset

This dataset consists of three csv files which are BX-Users.csv, BX-Books.csv, BX-Book-Ratings.csv.

## Merging Datasets

Using merge() function we merge BX-Book-Ratings.csv and BX-Books.csv by 'itemID'. The resultant dataset is merged with BX-Users.csv by userID which is book's ISBN. After merging the files we received a dataset with 1 031 175 rows

## Dataset Preprocessing 

We decided to generate 4 extra columns which will store only continuous values. They will be needed for the linear model explained later. The new columns are userMean, itemMean, userRatingCount, itemRatingCount. This is one of the longest steps in the Problem 2.

Based on the recosystem package documentation, userID, itemID and ratings should be integer, integer and numeric types vectors, respectively. Moreover, id's should start from 0 or 1. In modify_id_s() function We accomplish this requirement. It could be done during the previous step while generating new columns, but We did not want to do the same computations again. 

At some point We wanted to use userMean, itemMean and other covariates like age and year or publisher for our linear model. For this we had to deal with the missing values which after previous steps were in different forms like "NULL"(string), NULL or "NA"(string). We do replace this kind of values to NA's in process_columns() function. We also dropped the location column assuming that it wouldn't contribute much to the model. 

After this we convert the categorical columns to factors in to_factors() function. At this point we are ready to convert these to dummies using factorsToDummies() function. We are faced with this error "Error: vector memory exhausted (limit reached?)". One of the solutions of this could be passing each column separately to get the dummies and then append it back to the dataset. We did not do that and decided to use only 4 features in our linear model.

## Linear Regression Model - qeLin()

As We mentioned before, we decided to keep our feature choice on the 4 columns: userMean, itemMean, userRatingCount and itemRatingCount. We used full dataset to train LM model and it did not take much time. The testAcc value is 2.17. 

## Ad-hoc method

As a goal we have to use covariates in our linear model. To get rid of its affect in the recosystem model we are supposed to subtract the predicted ratings from the actual ones. We will get residuals and will use them in out recosystem model. This method is just a trial. It seems like a good idea and We expect that it works.

## Recosystem

Our Reccosystem work is implemented within the reco_experiment() function. It accepts training and testing arguments. These are split into 80% and 20% of the full one.

We use data_memory() function and pass itemID, userID, ratings for the training set and itemID, userID for the test set(ratings are ignored here).

Then we use tune Reco's member function to find the best hyperparameters for the next training step. It uses cross validation to tune the model parameters. This was the longest step because we tried dimensions from 20 to 40. In addition, the default parameters to try are L1 and L2 regularization cost for user factors, L1 and L2 regularization cost for item factors and learning rate. Each of these have two parameters. The best combination of parameter values with minimum cross validated loss are dim = 40, costp_l1 = 0.1, costp_l2 = 0.01, costq_l1 = 0.1, costq_l2 = 0.1, lrate = 0.01 with loss_fun = 2.796951

After this We trained our model using the tuning parameter values found in the last step. During tuning and training the ‘recosystem’ package allows us to use multithreading which We took advantage from. We also saved our model matrices in reco_model.txt. 

At the end We predict and return our ratings using the model trained in the previous step.

We also tried to run another experiment with recosystem where we tuned the model using ratings without substracting predicted ratings from the linear model trained on covariates.

## Results

To appropriately use the effect of covariates we should add the predicted ratings from the linear model back to the predicted ratings received from Reco because we trained Reco using the residuals(actual_raings - lm_predicted_ratings).

We calculated the RMSE error comparing actual ratings and predicted ones by Reco. RMSE is equal to 2.7399579.

After running the second experiment without covariates effect the reported RMSE is equal to 3.7878082. This model is worse and We can see a positive effect of training the Reco model on ratings residuals.
