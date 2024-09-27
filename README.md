# Called-Strike-Probability-Production

# **Project Overview**

### **Objective**

The project aimed to use Trackman data to calculate the probability of a pitch being called a strike. The model was to be designed as context-neutral regarding the pitcher, batter, catcher, and umpire so that those effects can be explored separately. Once the model was created, the model script was to be put into production before the start of the season so that further work can be built off it.

### **Context**

In baseball, the strike zone is defined as the area over home plate from the midpoint between a batter's shoulders and the top of the uniform pants and a point just below the kneecap. Called strike probability is a fundamental component for key player performance metrics designed to evaluate skills like a hitter’s swing decisions, a pitcher’s ability to command the ball, and catcher framing - the ability of a catcher to receive a pitch in a way that makes it more likely for the pitch to be called a strike by the umpire. Because Trackman data is widely available at a variety of levels in both Amateur and Professional baseball, metrics derived from the data are both flexible and powerful means of supplementing the player evaluation and development processes within an organization.

### **Role**

As the assigned analyst, I was responsible for managing the full project including data wrangling, model testing, presentation of the results and at-scale integration.

### **Tools and Methodologies**

Data analysis was conducted using R for statistical computations with connections to SQL pipelines. The methodologies used included correlation analysis, variable testing, and generalized additive mixed modeling (GAMMs). 

# **The Approach and Process**

### **Data Collection**

Trackman from both the Major and Minor league level was organized from the 2017-2019 seasons, resulting in a dataset of roughly 14MM pitches after removing games flagged by a calibration process due to questionable ball tracking from the Trackman unit. Pitches where the batter swung were removed from the dataset since those are not pitches called a ball or strike by the umpire and therefore unable to be used in a training dataset.

### **Exploratory Analysis**

The data was initially explored using a correlation matrix to understand the relationships among ball flight characteristics, the location of the ball as it crosses the plate, and game state in relation to called strike probability. Below is a matrix displaying the initial variables thought to have some degree of predictive power. Other variables tested include pitcher release angle, the spin rate of the ball, whether or not a runner on base is stealing, among others. Given the many variables and circumstances that ultimately affect whether a pitch is called a strike, it became clear that exploring variable categorization, interactions, and smoothing techniques would be beneficial for identifying better predictors.

### Feature Engineering

After addressing various data quality issues, several variable interactions were created for testing. These included grouping similar pitch types and creating bins for larger continuous variables, such as batter height and plate location (height and side). Variations of these features were eventually optimized during model building.

### Model Iteration

A GAMM was chosen due to its interpretability, regularization, and flexibility for different variable types. Model features were first tested on partitioned data, with the best prospective models eventually being tested by on the full data set. Iterations included changes to various tuning to spline and tensor product smooths, different bin sizes for grouped variables, link functions, as well as alternate predictors.

### Model Evaluation

RMSE and AIC were the primary tools to compare the initial models. Once the best-performing variations were selected for further refinement, there was additional analysis for points of interest. For example, pitches with ≥ 90% and ≤ 10% chance of being called a strike and “edge” pitches defined as +/- 3 inches (the width of a baseball) around the zone were explored in error analysis since these subsets are of the most important to predict correctly. A graphic representation of the final called strike probabilities can be found below.

### roductionalization

Once the final model was created and the results presented to stakeholders, I worked with the Lead Data Engineer to introduce called strike probability into the relevant SQL tables. We integrated the R script into daily data loading processes, introducing a while loop that checks to ensure pertinent upstream processes have completed so that new data would be both available and ready. Finally, we added the script to the batch process on the virtual machine to ensure it runs at an appropriate time during our daily loading.

### **Challenges and Solutions**

The biggest challenges during this project were computational power and time. Without access to computational power beyond my local laptop, initial model runs proved to have too long of run times to feasibly complete the project within the time frame. I navigated this challenge by better partitioning the dataset as well as using the “BAM” function in the mgcv R package, which is better optimized to create GAMMs with large datasets. It was essential to maintain a sufficiently large dataset to capture a variety of pitch characteristics, locations, and levels of play to accurately model different situations. Time constraints were also a factor during the project. The model needed to be completed and in production two months after the project was assigned so that it could be used during the upcoming season. I managed the time constraint by creating a project roadmap upfront with dedicated checkpoints to help myself adhere to a proper timetable.

# **End Results and Recommendations**

### **Outcomes**

The model was successfully completed and integrated on-time. The project quickly lead to Tableau products for our coaching staff where they could see probabilities in real-time to help develop our players, and the work was used as a building block for other player performance metrics.

### **Next Steps**

Moving forward, called strike probabilities were used as a foundation for other propriety metrics. One that I had the pleasure building was catcher receiving, which used a second-level model aimed at quantifying an individual catcher’s influence on called strike probability by exploring catcher fixed effects. Furthermore, the probabilities were used to create a batter swing decision metric, which measures a player’s ability to choose pitches he is more likely to achieve a positive outcome with.

### **Conclusion**

Modeling called strike probability was a fundamental step in improving the quality of analytics in the organization. The insights and discussions from the model results were used to help drive coaching strategy and player feedback, and future projects led to additional insights affecting decision making across the organization.

### Future Considerations

Although the model proved to be a powerful tool, there was room to improve by considering other variables not available at the time. For example, where the catcher initially sets up his glove and how far it moves to receive the ball may have an effect on how the umpire calls the pitch. Additionally, interpretability was prioritized so that model predictions can be more easily explained to different audiences. Using other modeling techniques and higher-order interaction terms may lead to improved model performance. These items were addressed in future off-seasons when time permitted.
