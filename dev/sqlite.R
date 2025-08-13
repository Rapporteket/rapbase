# Testing with sqlite

con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "autoreport")

pre_query <- "DROP TABLE IF EXISTS `autoreport`;"

DBI::dbExecute(con, pre_query)

query <- paste0("CREATE TABLE `autoreport` (",
"  `id` varchar(32) DEFAULT NULL,",
"  `synopsis` varchar(74) DEFAULT NULL,",
"  `package` varchar(17) DEFAULT NULL,",
"  `fun` varchar(28) DEFAULT NULL,",
"  `params` text,",
"  `owner` varchar(14) DEFAULT NULL,",
"  `email` varchar(48) DEFAULT NULL,",
"  `organization` varchar(22) DEFAULT NULL,",
"  `terminateDate` varchar(10) DEFAULT NULL,",
"  `interval` varchar(7) DEFAULT NULL,",
"  `intervalName` varchar(11) DEFAULT NULL,",
"  `runDayOfYear` text,",
"  `type` varchar(12) DEFAULT NULL,",
"  `ownerName` varchar(26) DEFAULT NULL,",
"  `startDate` varchar(10) DEFAULT NULL",
");")

DBI::dbExecute(con, query)

DBI::dbDisconnect(con)
