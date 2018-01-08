if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)
this_sample_break <- Sys.getenv( "this_sample_break" )
wvs_cat <- get_catalog( "wvs" , output_dir = file.path( getwd() ) )
record_categories <- ceiling( seq( nrow( wvs_cat ) ) / ceiling( nrow( wvs_cat ) / 5 ) )
wvs_cat <- wvs_cat[ record_categories == this_sample_break , ]
lodown( "wvs" , wvs_cat )
if( any( grepl( "United(.*)States" , wvs_cat$full_url ) & wvs_cat$wave == 6 ) ){
library(lodown)
# examine all available WVS microdata files
wvs_cat <-
	get_catalog( "wvs" ,
		output_dir = file.path( getwd() ) )

# wave six only
wvs_cat <- subset( wvs_cat , grepl( "United(.*)States" , full_url ) & wave == 6 )
# download the microdata to your local computer


library(survey)

wvs_df <-
	readRDS( 
		file.path( getwd() , 
			"wave 6/F00003106-WV6_Data_United_States_2011_spss_v_2016-01-01.rds" ) 
	)

# construct a fake survey design
warning( "this survey design produces correct point estimates
but incorrect standard errors." )
wvs_design <- 
	svydesign( 
		~ 1 , 
		data = wvs_df , 
		weights = ~ v258
	)
wvs_design <- 
	update( 
		wvs_design , 
		
		one = 1 ,
		
		language_spoken_at_home =
			factor( v247 , 
				levels = c( 101 , 128 , 144 , 208 , 426 , 800 ) , 
				labels = c( 'chinese' , 'english' , 'french' , 
					'japanese' , 'spanish; castilian' , 'other' )
			) ,

		citizen = as.numeric( v246 == 1 ) ,
		
		task_creativity_1_10 = as.numeric( v232 ) ,
		
		work_independence_1_10 = as.numeric( v233 ) ,
		
		family_importance =
			factor( v4 , 
				labels = c( 'very' , 'rather' , 'not very' , 'not at all' ) 
			)
	)
sum( weights( wvs_design , "sampling" ) != 0 )

svyby( ~ one , ~ language_spoken_at_home , wvs_design , unwtd.count )
svytotal( ~ one , wvs_design )

svyby( ~ one , ~ language_spoken_at_home , wvs_design , svytotal )
svymean( ~ task_creativity_1_10 , wvs_design , na.rm = TRUE )

svyby( ~ task_creativity_1_10 , ~ language_spoken_at_home , wvs_design , svymean , na.rm = TRUE )
svymean( ~ family_importance , wvs_design , na.rm = TRUE )

svyby( ~ family_importance , ~ language_spoken_at_home , wvs_design , svymean , na.rm = TRUE )
svytotal( ~ task_creativity_1_10 , wvs_design , na.rm = TRUE )

svyby( ~ task_creativity_1_10 , ~ language_spoken_at_home , wvs_design , svytotal , na.rm = TRUE )
svytotal( ~ family_importance , wvs_design , na.rm = TRUE )

svyby( ~ family_importance , ~ language_spoken_at_home , wvs_design , svytotal , na.rm = TRUE )
svyquantile( ~ task_creativity_1_10 , wvs_design , 0.5 , na.rm = TRUE )

svyby( 
	~ task_creativity_1_10 , 
	~ language_spoken_at_home , 
	wvs_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE ,
	na.rm = TRUE
)
svyratio( 
	numerator = ~ task_creativity_1_10 , 
	denominator = ~ work_independence_1_10 , 
	wvs_design ,
	na.rm = TRUE
)
sub_wvs_design <- subset( wvs_design , v242 >= 65 )
svymean( ~ task_creativity_1_10 , sub_wvs_design , na.rm = TRUE )
this_result <- svymean( ~ task_creativity_1_10 , wvs_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ task_creativity_1_10 , 
		~ language_spoken_at_home , 
		wvs_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( wvs_design )
svyvar( ~ task_creativity_1_10 , wvs_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ task_creativity_1_10 , wvs_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ task_creativity_1_10 , wvs_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ citizen , wvs_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( task_creativity_1_10 ~ citizen , wvs_design )
svychisq( 
	~ citizen + family_importance , 
	wvs_design 
)
glm_result <- 
	svyglm( 
		task_creativity_1_10 ~ citizen + family_importance , 
		wvs_design 
	)

summary( glm_result )
library(srvyr)
wvs_srvyr_design <- as_survey( wvs_design )
wvs_srvyr_design %>%
	summarize( mean = survey_mean( task_creativity_1_10 , na.rm = TRUE ) )

wvs_srvyr_design %>%
	group_by( language_spoken_at_home ) %>%
	summarize( mean = survey_mean( task_creativity_1_10 , na.rm = TRUE ) )

}
