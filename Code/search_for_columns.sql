-- search all tables across schema
/* USE bca_clc_tst_int_ci
USE bca_clc_tst_int_jasper
USE bca_clc_tst_int_quartz */

USE bca_clc_tst_int_ci
SELECT      
	c.name  AS 'ColumnName'
,	ty.Name 'Data type'
,	t.name AS 'TableName'
FROM        
	sys.columns c
	  INNER JOIN    sys.tables  t  ON c.object_id = t.object_id
	  INNER JOIN 	sys.types 	ty ON c.user_type_id = tY.user_type_id
ORDER BY    
	3, 2, 1
-- search all views across schema
/* USE bca_clc_tst_int_ci
USE bca_clc_tst_int_jasper
USE bca_clc_tst_int_quartz */

SELECT      COLUMN_NAME AS 'ColumnName'
            ,TABLE_NAME AS  'TableName'
FROM        INFORMATION_SCHEMA.COLUMNS
WHERE       UPPER(COLUMN_NAME) LIKE '%ARTI%'
ORDER BY    TableName
            ,ColumnName; 