## model 1b smoke alarm presence

Xianghui Dong

The target variable created according to [model document](https://docs.google.com/document/d/1oJN-QwLVqFHOvrRNtW2KEAkNZ-PuFiqTwa8y3iXx1Sg/edit).

	Measure 2: model  (Jonathan / Andrew)
	target variable: % of fire incidents within a region that didn’t have smoke detectors (Xianghui create variable, push to J / A for model)
	all fire incidents in a region(tract), how many didn't have smoke detectors? 


- AlarmPresenceNFIRS.html [Rendered html link](https://cdn.rawgit.com/home-fire-risk/smoke_alarm_models/master/model_1b_nfirs_smokealarm_pres/AlarmPresenceNFIRS.html) 
	My previous analysis of smoke alarm presence information in NFIRS data
- model_1b.Rmd
	R code for calculating the variables. It 
	- reading NFIRS original data
	- filter home fires
	- merge with valid geocoded address
	- interpret fire detector field and count all the home fires whether smoke alarm was present for each Census tract
- alarm_presence_by_tract.csv
	result for each year and combined.
- Later there will be a visualization report showing the target variables and other NFIRS data information in map.	
