set output error
cls
//ssc install heatplot
//ssc install palettes, replace
//ssc install colrspace, replace
//ssc install DfBeta, replace
//ssc install asdoc


noisily: dis "ProjectId;depVarName';indepVarName;b;se;t;pvalue;dfl;dfu;Nobs;F;Prob>F;RMSE;R2;RSS;MSS;train_rmse;train_mae;train_mape;valid_rmse;valid_mae;valid_mape;"

foreach ProjectId in 1 28{
foreach indepVarName in "Polarity_Score" "Subjectivity_Score" {
	foreach depVarName in "Issues" "Reporters" "Assignees" "In_Progress_Minutes" "Resolution_Time_Minutes" "Completed_Issues" "Story_Points" "Total_Effort_Minutes" "Bugs" "Stories" "Improvements" "Technical_Tasks" "Efficiency_Index" "Improvement_Ratio" "Velocity" "Assignee_Workload" "Reporter_Activity_Ratio" "Bug_Density"	{

		clear all

		local ExitOnFirstResultRun = 1
		local ExcludeOutliers = 1
		local ShowScatter = 1
		local ShowScatterWithOutliers = 1
		local CalcMSE = 1
		local ShowResiduals = 1
		
		mkdir ".\Plots", public
		import delimited ".\Get_Sprint.csv", delimiter(comma) varnames(1) case(preserve) asdouble clear 
		
		drop if Project_Id != `ProjectId'
		// add random variable and sort
		/*Randomization: Sorting the dataset by the random number variable ensures that the observations are randomized. 
This is crucial for creating a representative and unbiased split. Without sorting, the observations may be ordered in a particular way that could introduce bias or affect the randomness of the split.
		Cutoff point determination: Sorting the dataset allows you to accurately determine the cutoff point between the training and validation sets. By sorting the dataset, the random number variable is arranged in ascending order. The cutoff point can be identified by choosing the desired proportion or number of observations for the validation set.
		Consistency: Sorting the dataset ensures that the split remains consistent. If you need to repeat the split operation or refer back to the split later, the sorted order provides consistency in the assignment of observations to the training and validation sets.*/
		set seed 1
		generate random_var = runiform()
		sort random_var

		// index all rows
		egen row_index = seq(), from(1)

		// divide dataset into training and validation dataset
		gen data_type = "Train"
		replace data_type="Valid" if (_n > _N*0.8)
		
		* Nomalize
		if inlist("`depVarName'", "In_Progress_Minutes","Resolution_Time_Minutes","Total_Effort_Minutes") {
			summ `depVarName'
			scalar depVarMean = r(mean)
			scalar depVarStd = r(sd)
			scalar depVarMin = r(min)
			scalar depVarMax = r(max)
			gen temp = (`depVarName' - depVarMin)/(depVarMax -depVarMin)
			replace `depVarName' = temp
		}
		
	
		/*
		egen Issues_estd = std(Issues)
		egen Issues_mean = mean(Issues)
		egen Issues_min = min(Issues)
		egen Issues_max = max(Issues)

		gen Issues_cen = (Issues - Issues_mean)
		gen Issues_std = (Issues - Issues_mean)/Issues_estd
		gen Issues_norm = (Issues - Issues_min)/(Issues_max - Issues_min)

		summarize Issues Issues_cen Issues_std Issues_norm //, detail
		histogram Issues_norm, kdensity normal 
		*/

		//If the kernel density estimate of the residuals shows a skewed or non-normal distribution, we can try applying transformations to the dependent variable or the independent variables to achieve a more normal distribution. Common transformations include logarithmic, square root, or inverse transformations.

		/*Robust Option: tune(#) is the biweight tuning constant.  The default is tune(7), meaning seven times the median absolute deviation from the median residual; see Methods and formulas in [R] rreg.  Lower tuning constants downweight outliers rapidly but may lead to unstable estimates (less than 6 is not recommended).  Higher tuning constants produce milder downweighting.*/

		*regress depvar [ indepvars ] [ if ] [ in ] [ weight ] [ , options 
		regress `depVarName' `indepVarName' if data_type=="Train", robust
		matrix results = r(table)'
		predict pred_train if data_type == "Train"
		predict pred_valid if data_type == "Valid"

		// calc train resid based on regression
		// gen resid_train = `depVarName' - pred_train if data_type == "Train" // same as using predict with residuals as option.
		// gen resid_valid = `depVarName' - pred_valid if data_type == "Valid" // same as using predict with residuals as option.
		predict resid_train if data_type=="Train", residuals
		predict resid_valid if data_type=="Valid", residuals

		summ resid_train
		scalar resid_train_mean = r(mean)
		scalar resid_train_sd = r(sd)
		gen is_outlier_train = (abs(resid_train - r(mean)) > 3 * r(sd)) if data_type == "Train"
		gen is_outlier_valid = (abs(resid_valid - r(mean)) > 3 * r(sd)) if data_type == "Valid"
		
		if `ExcludeOutliers' == 1 {
			drop resid* pred*
			
			regress `depVarName' `indepVarName' if data_type=="Train" & is_outlier_train==0, robust
			// matrix results = r(table)'
			
			// predict based on new regression without outliers
			predict pred_train if data_type == "Train"
			predict pred_valid if data_type == "Valid"
			predict resid_train if data_type=="Train", residuals
			predict resid_valid if data_type=="Valid", residuals
		}

			// generate variables used by scatter plots
			generate lo1 = pred_train - 1*resid_train_sd
			generate hi1 = pred_train + 1*resid_train_sd
			generate lo2 = pred_train - 2*resid_train_sd
			generate hi2 = pred_train + 2*resid_train_sd
			generate lo3 = pred_train - 3*resid_train_sd
			generate hi3 = pred_train + 3*resid_train_sd
			
			// we only want positive numbers for low lines, so that we can only see the first quadrant.
			replace lo1 = 0 if lo1 < 0
			replace lo2 = 0 if lo2 < 0
			replace lo3 = 0 if lo3 < 0
			
			count if data_type=="Train" & is_outlier_train==0
			local train_count = r(N)
			local train_bin = ceil(sqrt(r(N)))

			count if data_type=="Valid" & is_outlier_valid==0
			local valid_count = r(N)
			local valid_bin = ceil(sqrt(r(N)))
		
		
		if `ShowScatter' == 1 {	
//  twoway lfit [dependent variable] [independent variable] || scatter [dependent variable] [independent variable]
	twoway 	(lfitci `depVarName' `indepVarName' if data_type=="Train" & is_outlier_train==0) /// // the fitted value with a confidence interval for the mean.
			(scatter `depVarName' `indepVarName' if data_type=="Train" & is_outlier_train==0, ///
				msymbol(o) msize(small)) ///
			(scatter `depVarName' `indepVarName' if data_type=="Valid" & is_outlier_valid==0, ///
				msymbol(+) msize(small) color(orange)) ///
			(line lo1 hi1 lo2 hi2 lo3 hi3 `indepVarName', ///
					sort pstyle(p1line p1line p2line p2line p3line p3line) lpattern(dash_dot..) lwidth(0.1..)), ///
			xlabel(, labsize(vsmall)) ylabel(, labsize(vsmall))  ///
			legend(order(1 "95% CI" 2 "Linear Fit" 3 "`depVarName' Train" 4 "`depVarName' Valid") size(vsmall) rows(1) cols(1)) ///
			xtitle("`depVarName'", size(small)) ytitle("`indepVarName'", size(small)) ///
			graphregion(margin(0 0 0 0)) ///
			nodraw name(main, replace)
			
			twoway  (histogram `indepVarName' if data_type=="Train" & is_outlier_train==0, ///
						yline(0, lcolor(black) lwidth(thin)) ///
						vertical yscale(on) xscale(off) ///
						xlabel(,labsize(vsmall) nogrid) ylabel(,labsize(vsmall) nogrid) ///
						bin(`train_bin') color(blue%40) ///
						legend(off)) ///
					(histogram `indepVarName' if data_type=="Valid" & is_outlier_valid==0, ///
						vertical yscale(on) xscale(off) ///
						bin(`valid_bin') color(red%40) ///
						legend(off)), ///
					graphregion(margin(0 0 1 0)) ///
					fysize(15) ///
					xtitle(, size(small)) ytitle(, size(small)) ///
					nodraw name(his1, replace)

			twoway  (histogram `depVarName' if data_type=="Train" & is_outlier_train==0, ///
						xline(0, lcolor(black) lwidth(thin)) ///
						horizontal yscale(off) xscale(on) ///
						xlabel(,labsize(vsmall) nogrid) ylabel(,labsize(vsmall) nogrid) ///
						bin(`train_bin') color(blue%40) ///
						legend(order(1 "T" 2 "V") size(vsmall) rows(1) cols(1))) ///
					(histogram `depVarName' if data_type=="Valid" & is_outlier_valid==0, ///
						horizontal yscale(off) xscale(on) ///
						bin(`valid_bin') color(red%40) ///
						legend(on)), /// 
					graphregion(margin(1 0 0 0)) ///
					fxsize(15) ///
					xtitle(, size(small)) ytitle(, size(small)) ///
					nodraw name(his2, replace)
			
			// combine plots and leave the second cell of the graph matrix empty
			graph  combine his1 main his2, hole(2) commonscheme ///
				   title("{bf}Histogram - Scatter Plot", size(2.75) pos(11)) ///
				   subtitle("Project `ProjectId' - `indepVarName' vs. `depVarName'", size(2.2) pos(11))
			
			/* Save the plot as an image file */
			graph export ".\Plots\Project `ProjectId' - Scatter `indepVarName' vs `depVarName'.png", as(png) replace
		}

		if `ShowScatterWithOutliers' == 1 {
	twoway 	(lfitci `depVarName' `indepVarName' if data_type=="Train" & is_outlier_train==0) /// // the fitted value with a confidence interval for the mean.
			(scatter `depVarName' `indepVarName' if data_type=="Train" & is_outlier_train==0, ///
				msymbol(o) msize(small)) ///
			(scatter `depVarName' `indepVarName' if data_type=="Valid" & is_outlier_valid==0, ///
				msymbol(+) msize(small) color(orange)) ///
			(scatter `depVarName' `indepVarName' if data_type=="Train" & is_outlier_train==1, ///
				msymbol(o) msize(small) color(red) mlabel(row_index) mlabsize(tiny)) ///
			(scatter `depVarName' `indepVarName' if data_type=="Valid" & is_outlier_valid==1, ///
				msymbol(+) msize(small) color(red) mlabel(row_index) mlabsize(tiny)) ///
			(line lo1 hi1 lo2 hi2 lo3 hi3 `indepVarName', ///
					sort pstyle(p1line p1line p2line p2line p3line p3line) lpattern(dash_dot..) lwidth(0.1..)), ///
			xlabel(, labsize(vsmall)) ylabel(, labsize(vsmall))  ///
			legend(order(1 "95% CI" 2 "Linear Fit" 3 "`depVarName' Train" 4 "`depVarName' Valid" 5 "Train Outliers" 6 "Valid Outliers") size(vsmall) rows(1) cols(1)) ///
			xtitle("`depVarName'", size(small)) ytitle("`indepVarName'", size(small)) ///
			graphregion(margin(0 0 0 0)) ///
			nodraw name(main, replace)
			
			twoway  (histogram `indepVarName' if data_type=="Train" & is_outlier_train==0, ///
						yline(0, lcolor(black) lwidth(thin)) ///
						vertical yscale(on) xscale(off) ///
						xlabel(,labsize(vsmall) nogrid) ylabel(,labsize(vsmall) nogrid) ///
						bin(`train_bin') color(blue%40) ///
						legend(off)) ///
					(histogram `indepVarName' if data_type=="Valid" & is_outlier_valid==0, ///
						vertical yscale(on) xscale(off) ///
						bin(`valid_bin') color(red%40) ///
						legend(off)), ///
					graphregion(margin(0 0 1 0)) ///
					fysize(15) ///
					xtitle(, size(small)) ytitle(, size(small)) ///
					nodraw name(his1, replace)

			twoway  (histogram `depVarName' if data_type=="Train" & is_outlier_train==0, ///
						xline(0, lcolor(black) lwidth(thin)) ///
						horizontal yscale(off) xscale(on) ///
						xlabel(,labsize(vsmall) nogrid) ylabel(,labsize(vsmall) nogrid) ///
						bin(`train_bin') color(blue%40) ///
						legend(order(1 "T" 2 "V") size(vsmall) rows(1) cols(1))) ///
					(histogram `depVarName' if data_type=="Valid" & is_outlier_valid==0, ///
						horizontal yscale(off) xscale(on) ///
						bin(`valid_bin') color(red%40) ///
						legend(on)), /// 
					graphregion(margin(1 0 0 0)) ///
					fxsize(15) ///
					xtitle(, size(small)) ytitle(, size(small)) ///
					nodraw name(his2, replace)
			
			// combine plots and leave the second cell of the graph matrix empty
			graph  combine his1 main his2, hole(2) commonscheme ///
				   title("{bf}Histogram - Scatter Plot", size(2.75) pos(11)) ///
				   subtitle("Project `ProjectId' - `indepVarName' vs. `depVarName With Outliers'", size(2.2) pos(11))
			
			/* Save the plot as an image file */
			graph export ".\Plots\Project `ProjectId' - Scatter Outliers `indepVarName' vs `depVarName'.png", as(png) replace
		}
		
		if `ShowResiduals' == 1{
			count if data_type=="Train" & is_outlier_train==0
			local train_count = r(N)
			local train_bin = ceil(sqrt(r(N)))
			
			count if data_type=="Valid" & is_outlier_valid==0
			local valid_count = r(N)
			local valid_bin = ceil(sqrt(r(N)))
			
			// Create a scatter plot of residuals vs predicted values
			// Create a histogram of the residuals
			twoway 	(scatter resid_train pred_train if data_type=="Train" & is_outlier_train==0, ///
						msymbol(o) msize(small)) ///
					(scatter resid_valid pred_valid if data_type=="Valid" & & is_outlier_valid==0, ///
						msymbol(+) msize(small) color(orange)), ///
					legend(order(1 "Train Residual" 2 "Valid Residual") size(vsmall) rows(1) cols(1)) ///
					xlabel(, labsize(vsmall)) ylabel(, labsize(vsmall))  ///
					yline(0) ///
					xtitle("`depVarName'", size(small)) ytitle("`indepVarName'", size(small)) ///
					graphregion(margin(0 0 0 0)) nodraw name(main, replace)
					
			twoway  (histogram pred_train if data_type=="Train" & is_outlier_train==0, ///
						yline(0, lcolor(black) lwidth(thin)) ///
						vertical yscale(on) xscale(off) ///
						xlabel(,labsize(vsmall) nogrid) ylabel(,labsize(vsmall) nogrid) ///
						bin(`train_bin') color(blue%40) ///
						legend(off)) ///
					(histogram pred_valid if data_type=="Valid" & is_outlier_valid==0, ///
						vertical yscale(on) xscale(off) ///
						bin(`valid_bin') color(red%40)), ///
					graphregion(margin(0 0 1 0)) ///
					fysize(15) ///
					xtitle(, size(small)) ytitle(, size(small)) ///
					nodraw name(his1, replace) ///
					
			twoway  (histogram resid_train if data_type=="Train" & is_outlier_train==0, ///
						xline(0, lcolor(black) lwidth(thin)) ///
						horizontal yscale(off) xscale(on) ///
						xlabel(,labsize(vsmall) nogrid) ylabel(,labsize(vsmall) nogrid) ///
						bin(`train_bin') color(blue%40) ///
						legend(on)) ///
					(histogram resid_valid if data_type=="Valid" & is_outlier_valid==0, ///
						horizontal yscale(off) xscale(on) ///
						bin(`valid_bin') color(red%40) ///
						legend(order(1 "T" 2 "V") size(vsmall))), ///
					graphregion(margin(1 0 -5 0)) ///
					fxsize(20) ///
					xtitle(, size(small)) ytitle(, size(small)) ///
					nodraw name(his2, replace)		

			graph  	combine his1 main his2, ///
						hole(2) commonscheme scheme(white_tableau)  ///
						title("{bf}Residual Plot", size(2.75) pos(11)) ///
						subtitle("Project `ProjectId' - `indepVarName' vs. `depVarName'", size(2.2) pos(11))
			graph export ".\Plots\Project `ProjectId' - Residual Plot `indepVarName' vs `depVarName'.png", as(png) replace
		}

		//---------------------- Calculate MSE, MAE and MAPE
		if `CalcMSE'== 1{
			* Calculate Training RMSE MAE MAPE
			* Calculate RMSE
			egen train_mse = mean(resid_train^2)
			scalar train_rmse = sqrt(train_mse)

			* Calculate MAE
			egen train_mae = mean(abs(resid_train))

			* Calculate MAPE
			egen train_mape = mean(abs(resid_train) / `depVarName')  if `depVarName'!=0
			replace train_mape = train_mape * 100  // To express as a percentage

			* Calculate Validation RMSE MAE MAPE
			* Calculate RMSE
			egen valid_mse = mean(resid_valid^2)
			scalar valid_rmse = sqrt(valid_mse)

			* Calculate MAE
			egen valid_mae = mean(abs(resid_valid))

			* Calculate MAPE
			egen valid_mape = mean(abs(resid_valid) / `depVarName') if `depVarName'!=0
			replace valid_mape = valid_mape * 100  // To express as a percentage
						
		local FProb = Ftail(e(df_m), e(df_r), e(F))
		noisily: dis `ProjectId' ";`depVarName';`indepVarName';" results[1,1] ";" results[1,2] ";" results[1,3] ";" results[1,4] ";" results[1,5] ";" results[1,6] ";" e(N) ";" e(F) ";" `FProb' ";" e(rmse) ";" e(r2) ";" e(rss) ";" e(mss) ";" train_rmse ";" train_mae ";" train_mape ";" valid_rmse ";" valid_mae ";" valid_mape ";"
		}

		if `ExitOnFirstResultRun' == 1 {
			exit
		}
	}
}
}

exit

ds train_rmse train_mae train_mape valid_rmse valid_mae valid_mape
local myvars = r(varlist)




exit

//predict predicted_values, xb
/*
Residuals are the differences between the observed values and the predicted values from a regression model. They represent the unexplained variability in the dependent variable that is not accounted for by the independent variables in the model.
*/

predict residuals, residuals // calculate the residuals from a regression model 
//predict std_res, rstandard // calculates the standardized residuals
predict standard_error, stdp // calculates the standard errors of the coefficient estimates
//predict resid, resid
//predict cooksd, cooksd



exit
histogram residuals, kdensity normal
exit

/*
qnorm std_res, yline(0) ylab(0 1)
histogram residuals, kdensity normal 
*/


/* --------------- Homoscedastic Test
An important assumption is that the variance in the residuals must be homoscedastic or constant. 
The scatter plot illustrates residuals vs. predicted values. There should not be any observable pattern. 
 
rvfplot, yline(0)
*/

/*A non-graphical approach is to use estat hettest command that performs a Breusch-Pagan test.
The null hypothesis is the assumption that the residuals (which are the differences between the observed values and the fitted values in the model) are homoscedastic, which means that their variability is constant.

estat hettest 
*/

/*
The Ramsey Regression Equation Specification Error Test (RESET) is a statistical test used to assess whether a model has omitted any important variables. The test is based on the idea that if a model is missing important variables, then the residuals will be correlated with certain powers of the fitted values of the model. The test compares the model with and without the omitted variables to see if there is a significant difference between the two models (Content creation team, Ramsey RESET test, 2022). 

ovtest
*/

//------------------ Normality test
// Normality of residuals
/*
Use the Shapiro-Wilk test of normality to assess normality statistically. The null hypothesis for the test is normality, so a low p-value indicates that the observed data is unlikely under the assumption it was drawn from a normal distribution.

The Shapiro-Wilk test is only intended for relatively small samples. R's shapiro.test() is for samples of 5,000 or less, and Stata's swilk for 2,000 or less. The two versions will also return different but similar W statistics. Furthermore, as sample sizes grow, increasingly trivial departures from normality (which are almost always present in real data) will result in small p-values. For this reason, visual tests are more useful.
A small p-value leads us to reject the null hypothesis of normality.

predict res_std, rstandard
swilk res_std
*/

/*
Visual test
Use a Q-Q plot with standardized residuals from the model to assess normality visually.
A Q-Q (quantile-quantile) plot shows how two distributions' quantiles line up, with our theoretical distribution (e.g., the normal distribution) as the x variable and our model residuals as the y variable. If two distributions follow the exact same shape, we would expect a perfect line of points where y=x
in a Q-Q plot, but this is never the case. Instead, the points tend to follow a curve one way or the other or in both directions.
In general, when points are below the line y=x
, this means we have observed more data than expected by this quantile, and when it is above the line y=x
, we have observed less data than expected.
It is important to note that we do not check the distribution of the outcome, but rather the distribution of the model residuals. A non-normally distributed outcome is fine and sometimes violations of normality can be addressed by including a non-normal predictor (e.g., skewed, categorical)! Any transformations that we perform, however, are done on the actual variables, not on the residuals.
Create a Q-Q plot with qnorm and supply it with the standardized residuals (res_std), and make the reference line red with rlopts(lcolor(red)):

qnorm std_res, rlopts(lcolor(red)) aspect(1)
*/

/*
Q-Q Plots
Each of the plots that follow are composed of two plots. The density plot on the left shows the observed data as a histogram and as a gray density curve. The blue density curve is the normal distribution. On the right, the Q-Q plot shows the observed data as points and the line y=x in red. Select summary statistics are also provided.

The plots below are intended to serve as an incomplete reference for the patterns we often encounter in Q-Q plots. Some corrective actions that can be considered are also discussed. Most of the plots below use data that were generated from a single data generating process (e.g., rgamma()). In the social sciences especially, observed data are the result of many processes, so not only Q-Q plots based on real data look a bit messier than these plots, but corrections will also not so easily remedy violations of normality.
The data used to create each plot consist of 10,000 values and were scaled to have mean 0 and variance 1.

predict std_res, rstandard // calculates the standardized residuals
qnorm std_res, yline(0) ylab(0 1)
*/

// An assumption of the regression model that impacts the validity of all tests (p, t and F) is that residuals behave are normal distributed. 
// The residuals are plotted, the difference between the observed values and the predicted values using kdensity residual, normal command.
//histogram residuals, kdensity normal 



/*
Standardize Normal Probability Plot 
Using pnorm command it can be plottted the Standardize Normal Probability Plot. The plot checks for non-normality in the middle range of residuals. 

pnorm residuals
*/


/*
Using qnorm command the researchers plot the Quintile-normal plot on residuals. The plot check for non-normality in the tails of the data by comparing quintiles of residuals to quintiles of a normal distribution. 

qnorm residuals

*/


//------------------ Outlier
/*Outliers are data points with extreme values that can have a negative effect on our estimators.  In general, if a value falls more than 3 standard deviations away from the mean of the dataset, it is considered an outlier. This rule of thumb is since approximately 99% of the values in a normal distribution fall within 3 standard deviations of the mean. Therefore, values that fall outside of this range are relatively rare and may indicate an unusual or exceptional event.
To start avplots (added-variable plots) command is used to visualize the data.

avplots
*/


/* -NOT ALLOWD After Robust
DfBeta measures the influence of each observation on the coefficient of a particular independent variable in standard errors terms. An observation is influential if it has a significant effect on the coefficient. A case is an influential outlier according to inequality below where N is the sample size.
predict resid, resid
dfbeta
*/

/* -NOT ALLOWD After Robust
DfBeta
DfBeta measures the influence of each observation on the coefficient of a particular independent variable in standard errors terms. An observation is influential if it has a significant effect on the coefficient. A case is an influential outlier according to inequality below where N is the sample size.

DfBeta
generate cutoffdfbeta = abs(_dfbeta)>2/sqrt(e(N)) & e(sample)
drop if cutoffdfbeta = 1
*/


/* -NOT ALLOWD After Robust 
Cook's distance
It measures how much an observation influences the overall model or predicted values. It is a summary measure of leverage and high residuals.

predict cooksd, cooksd
summarize cooksd
generate leveragepoint=1 if cooksd>e(N)
drop if leveragepoint == 1
*/

/* -NOT ALLOWD After Robust 
Leverage
Leverage measures how much an observation influences regression coefficients.
High influence if leverage h>2 k/N  Where k is the number of parameters (including the intercept) and N is the sample size. A rule-of-thumb: Leverage goes from 0 to 1. A value closer to 1 or over 0.5 may indicate problems.

https://sscc.wisc.edu/sscc/pubs/RegDiag-Stata/no-outlier-effects.html
Leverage is a measure of the distance between individual values of a predictor and other values of the predictor. In other words, a point with high leverage has an x-value far away from the other x-values. Points with high leverage have the potential to influence our model estimates.

Leverage values range from 0 to 1. Various cutoffs exist for determining what is considered a large value. As an example, we can consider an observation as having large leverage if6

Leverage_i>2k/n where k is the number of predictors (including the intercept) and n is the sample size.

predict lev, leverage
generate lev_cutoff = 2 * (e(df_m) + 1) / e(N)
drop if lev_cutoff>0.5
*/


/*
Below is a correlation matrix for all variables in the model. Numbers are Pearson
correlation coefficients, go from -1 to 1. Closer to 1 means strong correlation. A negative
value indicates an inverse relationship (roughly, when one goes up the other goes down).
pwcorr Polarity_Score Bugs, star(0.05)
*/


/*
acprplot graphs an augmented component-plus-residual plot (a.k.a. augmented partial residual plot) as described by Mallows (1986). This seems to work better than the component-plus-residual plot for identifying nonlinearities in the data.
*/
//acprplot Polarity_Score, lowess






//scatter residual Polarity_Score if data_type=="Train"





  // Evaluate performance on validation data (e.g., RMSE)
//summarize Bugs^2 if data_type=="Valid", meanonly
//local rmse`i' = sqrt(r(mean))



* Residual analysis
//predict residuals, residual
summarize residuals
hist residuals, normal

/* Check for linearity and homoscedasticity
predict fitted_values, xb
rvfplot
*/

/* Check for multicollinearity
collin independent_vars
*/

* Check for influential observations
/*
estat imtest
estat hettest
*/

* Test for autocorrelation
*estat bgodfrey

* Check for heteroscedasticity
*hettest, white

* Validate assumptions with additional tests (if necessary)
* Example: Breusch-Pagan/Cook-Weisberg test for heteroscedasticity
*estat hettest, breslowcook

* Generate regression diagnostics
*estat diagnostics

* Generate regression statistics
*estat summary

* Save the regression results
estimates store regression_results

* Generate predictions
predict predicted_values, xb

/* Save the predictions
save "predicted_values.dta", replace
*/




