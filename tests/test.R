if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)
# examine all available WVS microdata files
wvs_cat <-
	get_catalog( "wvs" ,
		output_dir = file.path( getwd() ) )

# wave six only
wvs_cat <- subset( wvs_cat , grepl( "United(.*)States" , full_url ) & wave == 6 )
# download the microdata to your local computer
stopifnot( nrow( wvs_cat ) > 0 )

library(survey)

wvs_df <-
	readRDS( 
		file.path( getwd() , 
			"wave 6/F00003106-WV6_Data_United_States_2011_spss_v_2016-01-01.rds" ) 
	)

wvs_design <- 
	svydesign( 
		~ psu , 
		strata = ~ stratum , 
		data = wvs_df , 
		weights = ~ weight , 
		nest = TRUE 
	)
wvs_design <- 
	update( 
		wvs_design , 
		q2 = q2 ,
		never_rarely_wore_bike_helmet = as.numeric( qn8 == 1 ) ,
		ever_smoked_marijuana = as.numeric( qn47 == 1 ) ,
		ever_tried_to_quit_cigarettes = as.numeric( q36 > 2 ) ,
		smoked_cigarettes_past_year = as.numeric( q36 > 1 )
	)
sum( weights( wvs_design , "sampling" ) != 0 )

svyby( ~ one , ~ ever_smoked_marijuana , wvs_design , unwtd.count )
svytotal( ~ one , wvs_design )

svyby( ~ one , ~ ever_smoked_marijuana , wvs_design , svytotal )
svymean( ~ bmipct , wvs_design , na.rm = TRUE )

svyby( ~ bmipct , ~ ever_smoked_marijuana , wvs_design , svymean , na.rm = TRUE )
svymean( ~ q2 , wvs_design , na.rm = TRUE )

svyby( ~ q2 , ~ ever_smoked_marijuana , wvs_design , svymean , na.rm = TRUE )
svytotal( ~ bmipct , wvs_design , na.rm = TRUE )

svyby( ~ bmipct , ~ ever_smoked_marijuana , wvs_design , svytotal , na.rm = TRUE )
svytotal( ~ q2 , wvs_design , na.rm = TRUE )

svyby( ~ q2 , ~ ever_smoked_marijuana , wvs_design , svytotal , na.rm = TRUE )
svyquantile( ~ bmipct , wvs_design , 0.5 , na.rm = TRUE )

svyby( 
	~ bmipct , 
	~ ever_smoked_marijuana , 
	wvs_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE ,
	na.rm = TRUE
)
svyratio( 
	numerator = ~ ever_tried_to_quit_cigarettes , 
	denominator = ~ smoked_cigarettes_past_year , 
	wvs_design ,
	na.rm = TRUE
)
sub_wvs_design <- subset( wvs_design , qn41 == 1 )
svymean( ~ bmipct , sub_wvs_design , na.rm = TRUE )
this_result <- svymean( ~ bmipct , wvs_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ bmipct , 
		~ ever_smoked_marijuana , 
		wvs_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( wvs_design )
svyvar( ~ bmipct , wvs_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ bmipct , wvs_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ bmipct , wvs_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ never_rarely_wore_bike_helmet , wvs_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( bmipct ~ never_rarely_wore_bike_helmet , wvs_design )
svychisq( 
	~ never_rarely_wore_bike_helmet + q2 , 
	wvs_design 
)
glm_result <- 
	svyglm( 
		bmipct ~ never_rarely_wore_bike_helmet + q2 , 
		wvs_design 
	)

summary( glm_result )
library(srvyr)
wvs_srvyr_design <- as_survey( wvs_design )
wvs_srvyr_design %>%
	summarize( mean = survey_mean( bmipct , na.rm = TRUE ) )

wvs_srvyr_design %>%
	group_by( ever_smoked_marijuana ) %>%
	summarize( mean = survey_mean( bmipct , na.rm = TRUE ) )

