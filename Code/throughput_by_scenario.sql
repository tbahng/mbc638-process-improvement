-- This is a query for Apriori R18 8114 Geometry Extraction CR_CLOC_StockMachR001 geometry data

SELECT DISTINCT
	s.scenario_name							scenario	
,	CONVERT(date, sfb.last_saved_timestamp)	last_saved
,	COUNT(DISTINCT s.part_number)			num_parts
FROM
	bca_clc_tst_int_ci.dbo.scenario_facts_base 				sfb
	 INNER JOIN bca_clc_tst_int_ci.dbo.scenario 			s ON sfb.scenario_id = s.id
	 INNER JOIN bca_clc_tst_int_ci.dbo.vpe					v ON sfb.vpe_id = v.id
	 INNER JOIN bca_clc_tst_int_ci.dbo.process_group 		pg ON pg.id=sfb.process_group_id
	 LEFT JOIN bca_clc_tst_int_ci.dbo.process_facts_base	pfb ON sfb.id = pfb.scenario_facts_id
	 LEFT JOIN bca_clc_tst_int_ci.dbo.process_custom_output pco ON pfb.id = pco.process_facts_id
WHERE
	v.name = 'Geometry Extraction'
  AND pco.name In('finishedVolumeInches3')
  AND s.scenario_name In(paste_scenario_names)
  AND pco.numeric_value IS NOT NULL
GROUP BY
	s.scenario_name								
,	CONVERT(date, sfb.last_saved_timestamp)	
