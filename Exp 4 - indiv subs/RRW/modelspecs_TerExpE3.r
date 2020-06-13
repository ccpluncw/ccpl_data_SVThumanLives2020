###############################
#### MODEL SPECIFICATIONS #####
###############################

simpleModelList <- NULL

#fixedNSD
  columnName <- NULL
  df.code <- NULL
  GroupByVariables <- NULL
  parameter <- "nSD"
  ParameterName <- "nSD"
  parameterBounds <- c(1, 1, 0.001)

simpleModelList  <- rrwAddEffectToRRWModel(simpleModelList, parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

#freeDB
  columnName <- NULL
  df.code <- NULL
  GroupByVariables <- NULL
  parameter <- "db"
  ParameterName <- "db"
  parameterBounds <- c(0.5, 0, 0.001)

  simpleModelList  <- rrwAddEffectToRRWModel(simpleModelList, parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

#freeB
  columnName <- NULL
  df.code <- NULL
  GroupByVariables <- NULL
  parameter <- "b"
  ParameterName <- "b"
  parameterBounds <- c(100, 5, 0.01)

  simpleModelList  <- rrwAddEffectToRRWModel(simpleModelList, parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

#simpleVoverallSE
  columnName <- NULL
  df.code <- NULL
  GroupByVariables <- c("defaultKilled")
  ParameterName <- "s"
  parameter <- "s"
  parameterBounds <- c(0, 0, 0.001)

  simpleVOverallSEModelList  <- rrwAddEffectToRRWModel(simpleModelList, parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

#overallSE
#Here, we input the conditional statements for coding the dummy or effect variable.  X is the conditional, V is the value.
  x1 <- "defaultKilled == 'defaultLVI'"
  v1 <- 1
  x2 <- "defaultKilled == 'defaultHVI'"
  v2 <- -1
  x3 <- "default"
  v3 <- 0

  #this is the columnName of the dummy/effect variable
  columnName <- "overallSEColumn"
  #this dataframe contains the coding inforamtion of the dummy/effect variable
  df.code <- data.frame(logic = c(x1,x2,x3), value = c(v1,v2,v3))
  #here we have the grouping variable(s) that will be used by the ddply to create the summary dataset.
  GroupByVariables <- c("defaultKilled")
  #this is the name given to the parameter that will measure the effect of this dummy/effect variable
  ParameterName <- "sOverall"
  #this is the parameter name for the RRW model. There are specific names: s, b, nSD, db, da, vc.
  parameter <- "s"
  #These are the bounds of the parameter values: c(high, low, interval)
  parameterBounds <- c(0.8, -0.8, 0.001)

  #add them to an existing model: here we add them to the simple model to create the overall Start Effect Model
  overallSEModelList  <- rrwAddEffectToRRWModel(simpleModelList, parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)


allModels <- list(simpleModelList= simpleModelList, simpleVOverallSEModelList=simpleVOverallSEModelList, overallSEModelList = overallSEModelList)


fixedModelList <- NULL

#fixedNSD
  columnName <- NULL
  df.code <- NULL
  GroupByVariables <- NULL
  parameter <- "nSD"
  ParameterName <- "nSD"
  parameterBounds <- c(1, 1, 0.001)

fixedModelList  <- rrwAddEffectToRRWModel(fixedModelList, parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

#fixedDB
  columnName <- NULL
  df.code <- NULL
  GroupByVariables <- NULL
  parameter <- "db"
  ParameterName <- "db"
  parameterBounds <- c(0.24, 0.24, 0.001)

  fixedModelList  <- rrwAddEffectToRRWModel(fixedModelList, parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

#fixedB
  columnName <- NULL
  df.code <- NULL
  GroupByVariables <- NULL
  parameter <- "b"
  ParameterName <- "b"
  parameterBounds <- c(38.66, 38.66, 0.01)

  fixedModelList  <- rrwAddEffectToRRWModel(fixedModelList, parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)


fixedSEModelList <- NULL

#fixedNSD
  columnName <- NULL
  df.code <- NULL
  GroupByVariables <- NULL
  parameter <- "nSD"
  ParameterName <- "nSD"
  parameterBounds <- c(1, 1, 0.001)

fixedSEModelList  <- rrwAddEffectToRRWModel(fixedSEModelList, parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

#fixedDB
  columnName <- NULL
  df.code <- NULL
  GroupByVariables <- NULL
  parameter <- "db"
  ParameterName <- "db"
  parameterBounds <- c(0.26, 0.26, 0.001)

  fixedSEModelList  <- rrwAddEffectToRRWModel(fixedSEModelList, parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

#fixedB
  columnName <- NULL
  df.code <- NULL
  GroupByVariables <- NULL
  parameter <- "b"
  ParameterName <- "b"
  parameterBounds <- c(49.65, 49.65, 0.01)

  fixedSEModelList  <- rrwAddEffectToRRWModel(fixedSEModelList, parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

  #fixedS
  x1 <- "defaultKilled == 'defaultLVI'"
  v1 <- 1
  x2 <- "defaultKilled == 'defaultHVI'"
  v2 <- -1
  x3 <- "default"
  v3 <- 0

  columnName <- "overallSEColumn"
  df.code <- data.frame(logic = c(x1,x2,x3), value = c(v1,v2,v3))
  GroupByVariables <- c("defaultKilled")
  ParameterName <- "sOverall"
  parameter <- "s"
  parameterBounds <- c(0.1, 0.1, 0.01)

  fixedSEModelList  <- rrwAddEffectToRRWModel(fixedSEModelList, parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

    allFixedModels <- list(fixedModelList= fixedModelList, fixedSEModelList=fixedSEModelList)
