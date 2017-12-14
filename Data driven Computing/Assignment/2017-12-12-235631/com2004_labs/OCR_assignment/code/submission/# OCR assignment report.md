# OCR assignment report 
## Feature Extraction (Max 200 Words) 
My dimension reduction used the PCA system which took the parameters, Feature_Vector_Full, which was a matrix of feature vectors, and model which is a dictionary for storing the outputs of the model training data.
The method first checks if there are any eigen vectors stored in the model dictionary. If there are vectors in the model, then those vectors are used for the PCA dimension reduction. If not, then new vectors are generated using the Feature_Vector_Full. The eigen vectors generated would then be organized from the largest to smallest and finally would be used for the PCA dimension reduction. The first ten of the reduced data would then be the 10 best pixels to use and as such are returned.
The newly generated eigen vectors will then be store in the model dictionary so that they can be reused in this function.
My reasoning behind storing reusing the eigen vectors is that the pages that will be classified using this data will have varying amounts of noise. As the first has the least amount of noise it will provide the best vectors for the dimension reduction process.
## Classifier (Max 200 Words) 
My classifier was implemented using the n k nearest neighbours. These would be selected using all the features in the test and training data which in this case would be generated from the model dictionary mentioned above and the supplied page. The model dictionary contains information called fvectors_train and labels_train which would be our training data and our label data while the page which is a matrix containing a feature vector on each row to be classified will be our test data.
This data is then used to classify cosine distance. The nearest distance that is obtained is then used to generate an array of labels to classify the page information.
This classifier function ins then called in the classify page function which has the page and model as parameters and returns the labels.
## Error Correction (Max 200 Words) 
[Describe and justify the design of any post classification error   correction that you may have attempted.] 
## Performance 
The percentage errors (to 1 decimal place) for the development data are   as follows: 
‐ Page 1: [97.9% correct%]  ‐ Page 2: [97.8% correct] ‐ Page 3: [78.8% correct] ‐ Page 4: [50.1% correct]  ‐ Page 5: [32.6% correct]  ‐ Page 6: [25.2% correct] 
## Other information (Optional, Max 100 words) 
[Optional: highlight any significant aspects of your system that are   NOT covered in the sections above] reduction implementation 
