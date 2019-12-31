-- This is a query for Apriori R18 8114 Geometry Extraction CR_CLOC_StockMachR001 scenario data

SELECT DISTINCT
	s.scenario_name				scenario
FROM
	bca_clc_tst_int_ci.dbo.scenario_facts_base 				sfb
	 INNER JOIN bca_clc_tst_int_ci.dbo.scenario 			s ON sfb.scenario_id = s.id
	 INNER JOIN bca_clc_tst_int_ci.dbo.vpe					v ON sfb.vpe_id = v.id
	 INNER JOIN bca_clc_tst_int_ci.dbo.process_group 		pg ON pg.id=sfb.process_group_id
	 LEFT JOIN bca_clc_tst_int_ci.dbo.process_facts_base	pfb ON sfb.id = pfb.scenario_facts_id
	 --LEFT JOIN process_custom_output 						pco ON pfb.id = pco.process_facts_id
WHERE
	s.scenario_name LIKE '%paste_scenario_name%'
  AND v.name = 'Geometry Extraction'