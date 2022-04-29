CREATE TABLE `autoreport` (
  `j` JSON
  CHECK (JSON_VALID(j))
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;

INSERT INTO `autoreport` VALUES ('{"key1": "dummyValue"}');
