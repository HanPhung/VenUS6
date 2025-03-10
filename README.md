This project contains WINBUGS and R code for the VenUS 6 Network Meta-Analysis (NMA) model. The files include:

1.WINBUGS_basecase: The NMA model written for WinBUGS.
2.V6_AD_network2: Data table for studies with aggregated data.
3.VenUS6_basecase: R code for data arrangement to feed into the NMA model and run the model using the R2WiBUGS package. 
Note that due to data-sharing agreements, the Individual Patient Data (IPD) for VenUS I and VenUS 6 are not included.


Additionally, the project includes R code and data for the VenUS 6 economic model. The files are as follows:

1.V6_basecase Jan25: Decision model.
2.Recur_lognormal: Parameters for recurrence (Log-normal distribution).
3.Chol_matrix: Variance-covariance matrix for recurrence.
4.V6 mortality: Life table.
5.coda1: The NMA output for relative treatment effectiveness (hazard ratio).
6.Resource_use: Data tables for resource use.
