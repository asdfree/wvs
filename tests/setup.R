if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)

wvs_cat <-
	get_catalog( "wvs" ,
		output_dir = file.path( getwd() ) )

# sample 40% of the records
which_records <- sample( seq( nrow( wvs_cat ) ) , round( nrow( wvs_cat ) * 0.40 ) )

# always sample the united states wave == 6 file
wvs_cat <- unique( rbind( wvs_cat[ which_records , ] , subset( wvs_cat , grepl( "United(.*)States" , full_url ) & wave == 6 ) ) )

lodown( "wvs" , wvs_cat )
