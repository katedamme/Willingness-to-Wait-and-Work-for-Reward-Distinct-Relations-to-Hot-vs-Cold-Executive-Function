####Data Analyses Decription####
###SEM model: all observed variables; comparing two tasks: task one (Nback) had two conditions (Neg, Neu), task two (EEfRT) had three conditions (12% 50% 88%)
###

###To examine the relationships between the two levels of executive function (Neutral N-Back, Negative N-Back), and the three levels of probability in willingness to wait (12%, 50%, 88%), we compared three structural equation models that described the relation between executive function and willingness to work.
### Manuscript Text Describing Analytic Approach:
# "The first model constrained the relations among the levels to be equal for both the willingness to work and executive function to describe the average relation between willingness to wait and executive function, to assess the general relation between taskperformance. 
# The second model examined the general relation of executive function across levels of the willingness to work conditions, by allowing the levels of willingness to work task to vary in magnitude and constraining the levels of executive function to be equal. 
# The third model examined the general relation of the willingness to work conditions across levels of executive function, by allowing the levels of executive function task to vary in magnitude and constraining willingness to work levels to be equal.
# In the final model, all paths were unconstrained to examine any patterned interactions across levels of both executive function and willingness to work."

### Interpretation of Difference in Model fit:
# If the first model fits, best this would suggest that there is a significant general relationship between executive function and willingness to work, that does not vary by condition level (model 2/3) or magnitude of the relations between levels (model 4). 
# If the second model is the best fit, this would imply that the relation between executive function and willingness to work varies across the levels of the probability of reward, but not levels of executive function (model 3). 
# If the third model is the best fit, then this would imply that the relation between executive function and willingness to work varies across the levels of the executive function levels.
# The final model was entirely unconstrained, and if this model is the best fit that would imply that there is a significant variance across the levels. This approach allowed us to fully examine all levels of both the N-Back and EEfRT tasks.

#### Data and Intercorr Info ####
SEMCABN_data<-read.csv(file = "20180421_SEM.csv", header = TRUE)
View(SEMCABN_data)
library(psych)
library(lavaan)
library(semPlot)
describe(SEMCABN_data)
###Intercorrelation table instead of raw data option
SEMCABN_cor<-lowerCor(SEMCABN_data[2:6])
data.cor = lav_matrix_lower2full(c(1,.63,1,.14,.41,1,.19,.24,-.04,1,.18,.20,-.08,.74,1))
rownames(data.cor) = colnames(data.cor) = c("EEfRT_88","EEfRT_50", "EEfRT_12", "NBack_Neg","NBack_Neu")
colnames(data.cor) = c("EEfRT_88","EEfRT_50", "EEfRT_12", "NBack_Neg","NBack_Neu")
data.cov<-cov(SEMCABN_data[2:6])
#### Model 1 (Average Relation between tasks in general): using actual data, but cov data above could be used ####
#### General Impact NBack to General EEfRT: Constrain 2 Nback conditions as equal; Constrain 3 EEfRT conditions as equal; two latent variable terms a (NBack general) to term b (EEfRT)
model1 = '
EEfRT_88~(a)*NBack_Neu+(a)*NBack_Neg
EEfRT_12~(a)*NBack_Neu+(a)*NBack_Neg
EEfRT_50~(a)*NBack_Neu+(a)*NBack_Neg
'
model1.fit = cfa(model1, data = SEMCABN_data[2:6], sample.nobs = 229, std.lv = TRUE, orthogonal = FALSE, missing = "fiml")
summary (model1.fit, standardized=TRUE, fit.measures = TRUE)
semPaths(model1.fit, whatLabel = "stand", layout = "tree" )
semPaths(model1.fit, whatLabel = "stand", layout = "spring")

#### Model 2 (Interaction by EEfRT Condition) ####
#### General NBack Across Levels of EEfRT (NBack by Level of EEfRT): Constrain NBack conditions to be equal; 3 EEfRT conditions are not constrained; three latent variable relations one for each level of EEfRT to a General NBack
model2 = '
EEfRT_12~(b)*NBack_Neu+(b)*NBack_Neg
EEfRT_50~(c)*NBack_Neu+(c)*NBack_Neg
EEfRT_88~(d)*NBack_Neu+(d)*NBack_Neg
'
model2.fit = cfa(model2, data = SEMCABN_data[2:6], sample.nobs = 229, std.lv = TRUE, orthogonal = FALSE, missing = "fiml")
summary (model2.fit, fit.measures = TRUE, standardized=TRUE)
semPaths(model2.fit, whatLabel = "stand", layout = "spring" )
semPaths(model2.fit, whatLabel = "stand", layout = "tree" )


#### Model 3 (Interaction by Nback Condition) #### 
#### General EEfRT Across Levels of NBack (NBack by Level of NBack): Constrain 3 EffRT levels to be equal; 2 Nack conditions are unconstrained; three latent variable relations one for each level of NBack to a General EEfRT

model3 = '
EEfRT_12~(e)*NBack_Neg+(f)*NBack_Neu
EEfRT_88~(e)*NBack_Neg+(f)*NBack_Neu
EEfRT_50~(e)*NBack_Neg+(f)*NBack_Neu
'
model3.fit = cfa(model3, data = SEMCABN_data[2:6], sample.nobs = 229, std.lv = TRUE, orthogonal = FALSE, missing = "fiml")
summary (model3.fit, fit.measures = TRUE, standardized=TRUE)
modificationindices(model3.fit, sort.=T)
semPaths(model3.fit, whatLabel = "stand", layout = "tree")
semPaths(model3.fit, whatLabel = "stand", layout = "spring")

#### Model 4 (Interaction of diffences in NBack conditions x EEfRT) ####
#### General Unconstrained Model of EEfRT to NBack to account for the levels of the ineratctions 
model4 = '
EEfRT_12~NBack_Neu+NBack_Neg
EEfRT_50~NBack_Neu+NBack_Neg
EEfRT_88~NBack_Neu+NBack_Neg
'
model4.fit = cfa(model4, data = SEMCABN_data[2:6], sample.nobs = 229, std.lv = TRUE, orthogonal = FALSE, missing = "fiml")
summary (model4.fit, fit.measures = TRUE, standardized=TRUE)
semPaths(model4.fit, whatLabel = "par", layout = "tree")
semPaths(model4.fit, whatLabel = "stand", layout = "spring")

#### Model Comparisons ####
anova(model1.fit, model2.fit)
anova(model1.fit, model3.fit)
anova(model2.fit, model4.fit)
anova(model2.fit, model4.fit)

### Compared to MPLUS OUTPUT from RZ: MPLUS "MODEL RESULTS" under "STD Standardization" match the R "Regressions"; MPLUS "Chi-Square Test of Model Fit for the Baseline Model" math the R "Model test baseline model"
### Model fit assessed by 'anova' function is consistent with the Stats Table pdf range RZ provided
