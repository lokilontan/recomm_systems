# Report PROBLEM 1

## mlb: 
After building the script we tried it on two datasets. The first one is mlb dataset. The smallest p-value has the coefficient of "Height" feature. The model trained only on "Height" showed the worst results, which makes sense. When the number of predictors with the smallest p-values increased, we observed better results in testAcc. It did not change a lot from i = 3; however, it is the best for i = 5 and i = 8. The best model in this case will be with smaller amount of features. These features are: "Height", "Age", "Position.Shortstop", "Position.Second_Baseman", "Position.First_Baseman" and MAPE = 13.12127

## pef: 
In the dataset pef the smaller p-value  is for coefficient "wkswrkd". The best model for this dataset is the one with 8 features "wkswrkd", "educ.14", "age", "educ.16", "sex.1", "occ.100", "occ.101", "occ.106". The testAcc value for this one is 25289.76
