####Data Analyses Decription####
###SEM model: all observed variables; comparing two tasks: task one (Nback) had two conditions (Neg, Neu), task two (DDT-k) had 6 conditions ( two weeks, one month, two months, six months, one year, three years, or ten years )
###

###To examine the relationships between the two levels of executive function (Neutral N-Back, Negative N-Back), and the three 6 of delay periods in willingness to wait (two weeks, one month, two months, six months, one year, three years, or ten years ), we compared three structural equation models that described the relation between executive function and willingness to wait
### Manuscript Text Describing Analytic Approach:
# "The first model constrained the relations among the levels to be equal for both the willingness to wait and executive function to describe the average relation between willingness to wait and executive function, to assess the general relation between task performance. 
# The second model examined the general relation of executive function across levels of the willingness to wait conditions, by allowing the levels of willingness to wait task to vary in magnitude and constraining the levels of executive function to be equal. 
# The third model examined the general relation of the willingness to wait conditions across levels of executive function, by allowing the levels of executive function task to vary in magnitude and constraining willingness to wait levels to be equal.
# In the final model, all paths were unconstrained to examine any patterned interactions across levels of both executive function and willingness to wait"

### Interpretation of Difference in Model fit:
# If the first model fits, best this would suggest that there is a significant general relationship between executive function and willingness to wait, that does not vary by condition level (model 2/3) or magnitude of the relations between levels (model 4). 
# If the second model is the best fit, this would imply that the relation between executive function and willingness to wait varies across the delay periods, but not levels of executive function (model 3). 
# If the third model is the best fit, then this would imply that the relation between executive function and willingness to wait varies across the levels of the executive function levels.
# The final model was entirely unconstrained, and if this model is the best fit that would imply that there is a significant variance across the levels. This approach allowed us to fully examine all levels of both the N-Back and DDT tasks.

#### Data and Intercorr Info ####
SEMCABN_data<-read.csv(file = "/Users/damme/Desktop/PapersIP/BrainMAPD_EF_EEfRT_Nbk/Revision_Drafts/Revision2/New", header = TRUE)
View(SEMCABN_data)
library(psych)
library(lavaan)
library(semPlot)
describe(SEMCABN_data)
###Intercorrelation table instead of raw data option
SEMCABN_cor<-lowerCor(SEMCABN_data[2:9])
#DDT_SV_2W DDT_SV_4 DDT_SV_25 DDT_SV_52W DDT_SV_1 DDT_SV_520 NbckNg.ACC NbckNt.ACC
#DDT_SV_2Weeks      1.00                                                                            
#DDT_SV_4Weeks      0.49      1.00                                                                  
#DDT_SV_25Weeks     0.38      0.61     1.00                                                         
#DDT_SV_52Weeks     0.37      0.53     0.74      1.00                                               
#DDT_SV_156Weeks    0.29      0.44     0.60      0.66       1.00                                    
#DDT_SV_520Weeks    0.27      0.29     0.51      0.58       0.62     1.00                           
#NbackNegative.ACC  0.20      0.22     0.25      0.13       0.14     0.03       1.00                
#NbackNeutral.ACC   0.10      0.09     0.06      0.01       0.02    -0.07       0.74       1.00  
data.cor = lav_matrix_lower2full(c(1,.49,1,.38,.61,1,.37,.53,.74,1,.29,.44,.6,.66,1,.27,.29,.51,.58,.62,1,.20,.22,.25,.13,.14,.03,1,.10,.09,.06,.01,.02,-.07,.74,1))
rownames(data.cor) = colnames(data.cor) = c("DDT_2Weeks","DDT_4Weeks", "DDT_25Weeks", "DDT_52Weeks", "DDT_156Weeks", "DDT_520Weeks","NBack_Neg","NBack_Neu")
colnames(data.cor) = c("DDT_2Weeks","DDT_4Weeks", "DDT_25Weeks", "DDT_52Weeks", "DDT_156Weeks", "DDT_520Weeks","NBack_Neg","NBack_Neu")
data.cov<-cov(SEMCABN_data[2:9])
#### Model 1 (Average Relation between tasks in general) ####
#### General Impact NBack to General EEfRT: Constrain 2 Nback conditions as equal; Constrain 6 DDT conditions as equal; two latent variable terms a (NBack general) to term b (DDT)
model1 = '
DDT_2Weeks~(a)*NBack_Neu+(a)*NBack_Neg
DDT_4Weeks~(a)*NBack_Neu+(a)*NBack_Neg
DDT_25Weeks~(a)*NBack_Neu+(a)*NBack_Neg
DDT_52Weeks~(a)*NBack_Neu+(a)*NBack_Neg
DDT_156Weeks~(a)*NBack_Neu+(a)*NBack_Neg
DDT_520Weeks~(a)*NBack_Neu+(a)*NBack_Neg
'
model1.fit = cfa(model1, data = SEMCABN_data[2:9], sample.nobs = 229, std.lv = TRUE, orthogonal = FALSE)
summary (model1.fit, standardized=TRUE, fit.measures = TRUE)
semPaths(model1.fit, whatLabel = "stand", layout = "tree" )
semPaths(model1.fit, whatLabel = "stand", layout = "spring")

#### Model 2 (Interaction by EEfRT Condition) ####
#### General NBack Across Levels of DDT (NBack by Level of DDT): Constrain NBack conditions to be equal; 6 DDT conditions are not constrained; three latent variable relations one for each level of DDT to a General NBack
model2 = '
DDT_2Weeks~(b)*NBack_Neu+(b)*NBack_Neg
DDT_4Weeks~(c)*NBack_Neu+(c)*NBack_Neg
DDT_25Weeks~(d)*NBack_Neu+(d)*NBack_Neg
DDT_52Weeks~(e)*NBack_Neu+(e)*NBack_Neg
DDT_156Weeks~(f)*NBack_Neu+(f)*NBack_Neg
DDT_520Weeks~(g)*NBack_Neu+(g)*NBack_Neg
'

model2.fit = cfa(model2, data = SEMCABN_data[2:9], sample.nobs = 229, std.lv = TRUE, orthogonal = FALSE)
summary (model2.fit, fit.measures = TRUE, standardized=TRUE)
semPaths(model2.fit, whatLabel ="par", intercepts = FALSE, layout = "spring" )
semPaths(model2.fit, whatLabel = "stand", layout = "tree" )


#### Model 3 (Interaction by Nback Condition) #### 
#### General DDT Across Levels of NBack (NBack by Level of NBack): Constrain 6 DDT levels to be equal; 2 Nack conditions are unconstrained; Six latent variable relations one for each level of NBack to a General DDT

model3 = '
DDT_2Weeks~(h)*NBack_Neu+(i)*NBack_Neg
DDT_4Weeks~(h)*NBack_Neu+(i)*NBack_Neg
DDT_25Weeks~(h)*NBack_Neu+(i)*NBack_Neg
DDT_52Weeks~(h)*NBack_Neu+(i)*NBack_Neg
DDT_156Weeks~(h)*NBack_Neu+(i)*NBack_Neg
DDT_520Weeks~(h)*NBack_Neu+(i)*NBack_Neg
'
model3.fit = cfa(model3, data = SEMCABN_data[2:9], sample.nobs = 229, std.lv = TRUE, orthogonal = FALSE)
summary (model3.fit, fit.measures = TRUE, standardized=TRUE)
modificationindices(model3.fit, sort.=T)
semPaths(model3.fit, whatLabel = "stand", layout = "tree")
semPaths(model3.fit, whatLabel = "stand", layout = "spring")

#### Model 4 (Interaction of diffences in NBack conditions x EEfRT) ####
#### General Unconstrained Model of DDT to NBack to account for the levels of the ineratctions 
model4 = '
DDT_2Weeks~NBack_Neu+NBack_Neg
DDT_4Weeks~NBack_Neu+NBack_Neg
DDT_25Weeks~NBack_Neu+NBack_Neg
DDT_52Weeks~NBack_Neu+NBack_Neg
DDT_156Weeks~NBack_Neu+NBack_Neg
DDT_520Weeks~NBack_Neu+NBack_Neg
'

model4.fit = cfa(model4, data = SEMCABN_data[2:9], sample.nobs = 229, std.lv = TRUE, orthogonal = FALSE)
summary (model4.fit, fit.measures = TRUE, standardized=TRUE)
semPaths(model4.fit, whatLabel = "stand", layout = "tree")
semPaths(model4.fit, whatLabel = "stand", layout = "spring")

#### Model Comparisons ####
anova(model1.fit, model2.fit)
anova(model1.fit, model3.fit)
anova(model3.fit, model4.fit)

### Compared to MPLUS OUTPUT from RZ: MPLUS "MODEL RESULTS" under "STD Standardization" match the R "Regressions"; MPLUS "Chi-Square Test of Model Fit for the Baseline Model" math the R "Model test baseline model"
### Model fit assessed by 'anova' function is consistent with the Stats Table pdf range RZ provided
