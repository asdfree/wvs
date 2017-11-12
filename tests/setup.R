if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

this_sample_break <- Sys.getenv( "this_sample_break" )

library(lodown)

wvs_cat <-
	get_catalog( "wvs" ,
		output_dir = file.path( getwd() ) )

record_categories <- ceiling( seq( nrow( wvs_cat ) ) / ceiling( nrow( wvs_cat ) / 5 ) )

wvs_cat <- unique( rbind( wvs_cat[ record_categories == this_sample_break , ] , wvs_cat[ grepl( "United(.*)States" , wvs_cat$full_url ) & wvs_cat$wave == 6 , ] ) )

lodown( "wvs" , wvs_cat )
