-- query the start and end time stamps for bulk costing in apriori
SELECT
	s.scenario_name					scenario_name
,	MIN(sfb.costed_timestamp) 		start_timestamp
,	MAX(sfb.last_saved_timestamp) 	end_timestamp
FROM 
	bca_clc_tst_int_ci.dbo.scenario_facts_base sfb
	  INNER JOIN bca_clc_tst_int_ci.dbo.scenario s on ((s.id = sfb.scenario_id))
WHERE
	UPPER(s.scenario_name) IN(PASTE_SCENARIO_NAMES_HERE)
GROUP BY
	s.scenario_name		