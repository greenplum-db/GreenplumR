# GPTapply

getRandomNameList <- function(n = 1) {
  a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
}

db.gptapply <- function(X, INDEX, FUN = NULL, output.name=NULL, output.signature=NULL,
		clear.existing=FALSE, case.sensitive=FALSE,output.distributeOn=NULL,debugger.mode = FALSE, simplify = TRUE, language="plr", ...)
{	
	args <- list(...)

	randomName <- getRandomNameList()[1]

	typeName <- sprintf("gptype_%s", randomName)
	if (!is.null(output.signature))
	{
		typelist_str <- sprintf("CREATE TYPE %s AS (\n", typeName)
		typelist <- .simplify.signature(output.signature)
		fieldStr <- paste(names(typelist), typelist, sep=" ", collapse=",\n")
		typelist_str <- paste(typelist_str, fieldStr, "\n);", sep="")
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
	
	listStr <- .extract.param.list(args)
	if (nchar(listStr)>0)
		listStr <- paste(", ", listStr, sep="")

	funName <- sprintf("gprfunc_%s", randomName)
	funBody <- paste("# container:  plc_r_shared\ngplocalf <- ",paste(deparse(FUN), collapse="\n"))
	localdf <- sprintf("df <- data.frame(%s)\n", local_data_frame_str)
	localcall <- sprintf("do.call(gplocalf, list(df %s))", listStr);

	createStmt <- sprintf("CREATE FUNCTION %s (%s) RETURNS SETOF %s AS $$ %s\n %s\ return(%s)\n $$ LANGUAGE '%s';",
	funName, func_para_str, typeName, funBody, localdf, localcall, language);

	#print(createStmt)
	db.q(createStmt)

	if (is.null(output.name))
	{
		query <- sprintf("WITH a AS (SELECT (%s(%s)) AS b FROM (SELECT %s FROM %s GROUP BY %s) tmptbl) SELECT (b::%s).* FROM a;",
				funName, call_udf_params, call_udf_inner_params, relation_name, INDEX, typeName)
	}
	else
	{
		query <- sprintf("CREATE TABLE %s AS WITH a AS (SELECT (%s(%s)) AS b FROM (SELECT %s FROM %s GROUP BY %s) tmptbl) SELECT (b::%s).* FROM a;",
				output.name, funName, call_udf_params, call_udf_inner_params, relation_name, INDEX, typeName)
	}
	#print(query)

	results <- db.q(query, nrows = NULL)

	cleanString <- sprintf("DROP TYPE %s CASCADE;",typeName)

	print(cleanString)
	db.q(cleanString)

	return(results)

}
