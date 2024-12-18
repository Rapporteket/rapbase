DROP TABLE IF EXISTS `autoreportTest`;
CREATE TABLE `autoreportTest` (
  `id` varchar(32) COLLATE utf8mb3_danish_ci DEFAULT NULL,
  `synopsis` varchar(74) COLLATE utf8mb3_danish_ci DEFAULT NULL,
  `package` varchar(17) COLLATE utf8mb3_danish_ci DEFAULT NULL,
  `fun` varchar(28) COLLATE utf8mb3_danish_ci DEFAULT NULL,
  `params` text COLLATE utf8mb3_danish_ci,
  `owner` varchar(14) COLLATE utf8mb3_danish_ci DEFAULT NULL,
  `email` varchar(48) COLLATE utf8mb3_danish_ci DEFAULT NULL,
  `organization` varchar(22) COLLATE utf8mb3_danish_ci DEFAULT NULL,
  `terminateDate` varchar(10) COLLATE utf8mb3_danish_ci DEFAULT NULL,
  `interval` varchar(7) COLLATE utf8mb3_danish_ci DEFAULT NULL,
  `intervalName` varchar(11) COLLATE utf8mb3_danish_ci DEFAULT NULL,
  `runDayOfYear` text COLLATE utf8mb3_danish_ci,
  `type` varchar(12) COLLATE utf8mb3_danish_ci DEFAULT NULL,
  `ownerName` varchar(26) COLLATE utf8mb3_danish_ci DEFAULT NULL,
  `startDate` varchar(10) COLLATE utf8mb3_danish_ci DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb3 COLLATE=utf8mb3_danish_ci;
