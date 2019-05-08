# GPTapply

getRandomNameList <- function(n = 1) {
  a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
}

db.gptapply <- function(X, INDEX, FUN = NULL, output.name=NULL, output.signature=NULL,
		clear.existing=FALSE, case.sensitive=FALSE,output.distributeOn=NULL,debugger.mode = FALSE, simplify = TRUE, ...)
{	
	args <- list(...)

	randomName <- getRandomNameList()[1]

	typeName <- sprintf("gptype_%s", randomName)
	if (!is.null(output.signature))
	{
		typelist_str <- sprintf("CREATE TYPE %s AS (\n", typeName)
		typelist <- output.signature()
		for( i in 1:length(typelist))
		{
			if (i < length(typelist))
			{
				typelist_str <- paste(typelist_str, names(typelist)[i], " ", typelist[[i]], ",\n", sep="")
			}
			else
			{
				typelist_str <- paste(typelist_str, names(typelist)[i], " ", typelist[[i]], "\n", sep="")
			}
			
		}
		typelist_str <- paste(typelist_str, ");", sep="")
		print(typelist_str)
		db.q(typelist_str)

	}
	
	# generate function parameter str
	ar <- attributes(X)
	func_para_str <- ""
	local_data_frame_str <- ""
	call_udf_params <- ""
	call_udf_inner_params <- ""
	relation_name <- ar$.content

	if(is.null(INDEX))
	{
		stop("INDEX value cannot be NULL");
	}
	else if (is.integer(INDEX) || (is.double(INDEX) && floor(INDEX) == INDEX))
	{
		INDEX <- ar$.col.name[INDEX];
	}
	else if(!is.character(INDEX))
	{
		stop("INDEX value cannot be determined with value of type ", typeof(INDEX))
	}

	for (i in 1:length(ar$.col.name))
	{
		if (i > 1)
		{
			
			local_data_frame_str <- paste(local_data_frame_str, ",", ar$.col.name[i], "=", ar$.col.name[i], sep="")
			call_udf_params <- paste(call_udf_params, ", ", ar$.col.name[i], sep="")
			if(toupper(ar$.col.name[i]) == toupper(INDEX))
			{
				call_udf_inner_params <- paste(call_udf_inner_params, ", ", ar$.col.name[i], sep="")
				func_para_str <- paste(func_para_str, ", ", ar$.col.name[i], " ", ar$.col.udt_name[i], sep="")
			}
			else
			{
				call_udf_inner_params <- paste(call_udf_inner_params, " , array_agg(", ar$.col.name[i], ") AS ", ar$.col.name[i], sep="")
				func_para_str <- paste(func_para_str, ", ", ar$.col.name[i], " ", ar$.col.udt_name[i], "[]", sep="")
			}
		}
		else
		{
			local_data_frame_str <- paste(local_data_frame_str, ar$.col.name[i], "=", ar$.col.name[i], sep="")
			call_udf_params <- paste(call_udf_params, ar$.col.name[i], sep="")

			if(toupper(ar$.col.name[i]) == toupper(INDEX))
			{
				call_udf_inner_params <- paste(call_udf_inner_params, ar$.col.name[i], sep="")
				func_para_str <- paste(func_para_str, ar$.col.name[i], " ", ar$.col.udt_name[i], sep="")
			}
			else
			{
				call_udf_inner_params <- paste(call_udf_inner_params, "array_agg(", ar$.col.name[i], ") AS ", ar$.col.name[i], sep="")
				func_para_str <- paste(func_para_str, ar$.col.name[i], " ", ar$.col.udt_name[i], "[]", sep="")
			}
		}
	}
	#print(call_udf_params)
	#print(call_udf_inner_params)
	
  arg_str_array <- strsplit(deparse(args), ", .Names = ")[[1]]
	#print(length(arg_str_array))
	if (length(arg_str_array) == 1)
	{
		listStr = substr(arg_str_array[1], 6, nchar(arg_str_array[1]) - 1)
	}
	else if (length(arg_str_array) == 2)
	{
	   listStr = substr(arg_str_array[1], 16, nchar(arg_str_array[1]) - 1)
	}
	else
	{
		stop("The functon input argument must not inlcude '.Names'")
	}

	funName <- sprintf("gprfunc_%s", randomName)
	funBody <- paste("# container:  plc_r_shared\ngplocalf <- ",paste(deparse(FUN), collapse="\n"))
	localdf <- sprintf("df <- data.frame(%s)\n", local_data_frame_str)
	localcall <- sprintf("do.call(gplocalf, list(df, %s))", listStr);

	createStmt <- sprintf("CREATE FUNCTION %s (%s) RETURNS SETOF %s AS $$ %s\n %s\ return(%s)\n $$ LANGUAGE 'plcontainer';",
	funName, func_para_str, typeName, funBody, localdf, localcall);

	#print(createStmt)
	db.q(createStmt)

	if (is.null(output.name))
	{
		query <- sprintf("SELECT 
      					(%s(%s)).*
					FROM (
  						SELECT %s
  						FROM %s 
  						GROUP BY %s
					) foo;", 
					funName, call_udf_params,call_udf_inner_params, relation_name, INDEX)
	}
	else
	{
		query <- sprintf("CREATE TABLE %s AS SELECT 
      					(%s(%s)).*
					FROM (
  						SELECT %s
  						FROM %s 
  						GROUP BY %s
					) foo;", 
					output.name, funName, call_udf_params,call_udf_inner_params, relation_name, INDEX)
	}
	#print(query)

	results <- db.q(query, nrows = NULL)

	cleanString <- sprintf("DROP TYPE %s CASCADE;",typeName)

	print(cleanString)
	db.q(cleanString)

	return(results)

}
