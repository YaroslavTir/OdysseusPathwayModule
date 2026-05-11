/* Pre-Index Pathway Analysis
 * Adapted from OHDSI/WebAPI RunPathwayAnalysis.sql
 * Original: https://github.com/OHDSI/WebAPI/blob/fe070e527abe61a59c42c23e69121f82dff5b4f1/src/main/resources/resources/pathway/runPathwayAnalysis.sql
 *
 * The core implementation of CohortPathway analysis was developed by
 * Christopher Knoll (github: chrisknoll), and enhanced by members of the
 * OHDSI community: mick-iqvia (github), Odysseus Inc., and many other
 * collaborators on the OHDSI/WebAPI team.
 *
 * Modification: Instead of filtering events within the target cohort period
 * (start_date to end_date), this version filters events within a lookback
 * window BEFORE the target cohort start date (index date).
 * Parameters @lookback_start_day and @lookback_end_day define the window
 * relative to the index date (e.g., -365 and -1).
 */

/*
 * Filter events to those falling in the PRE-INDEX lookback window
 * [index_date + lookback_start_day, index_date + lookback_end_day]
 * e.g. for -365 to -1: events in the year before the index date
 */

select event_cohort_index, subject_id, CAST(cohort_start_date AS DATETIME) AS cohort_start_date, CAST(cohort_end_date AS DATETIME) AS cohort_end_date
INTO {@combo_window != 0 }?{ #raw_events }:{#event_cohort_eras}
FROM (
	SELECT ec.cohort_index AS event_cohort_index,
	  e.subject_id,
	  e.cohort_start_date,
	  dateadd(d, 1, e.cohort_end_date) as cohort_end_date
	FROM @event_cohort_table e
	  JOIN ( @event_cohort_id_index_map ) ec ON e.cohort_definition_id = ec.cohort_definition_id
	  JOIN @target_cohort_table t
	    ON t.subject_id = e.subject_id
	    AND t.cohort_definition_id = @pathway_target_cohort_id
	    AND e.cohort_start_date >= dateadd(d, @lookback_start_day, t.cohort_start_date)
	    AND e.cohort_start_date <= dateadd(d, @lookback_end_day, t.cohort_start_date)
) RE;

{@combo_window != 0 }?{-- Begin Collapse Events
/*
 * Find closely located dates, which need to be collapsed, based on combo_window
 */

WITH person_dates AS (
  SELECT subject_id, cohort_start_date cohort_date FROM #raw_events
  UNION
  SELECT subject_id, cohort_end_date cohort_date FROM #raw_events
),
marked_dates AS (
  SELECT ROW_NUMBER() OVER (ORDER BY subject_id ASC, cohort_date ASC) ordinal,
    subject_id,
    cohort_date,
    CASE WHEN (datediff(d,LAG(cohort_date) OVER (ORDER BY subject_id ASC, cohort_date ASC), cohort_date) < @combo_window AND subject_id = LAG(subject_id) OVER (ORDER BY subject_id ASC, cohort_date ASC)) THEN 1 ELSE 0 END to_be_collapsed
  FROM person_dates
),
grouped_dates AS (
  SELECT ordinal, subject_id, cohort_date, to_be_collapsed, ordinal - SUM(to_be_collapsed) OVER ( PARTITION BY subject_id ORDER BY cohort_date ASC ROWS UNBOUNDED PRECEDING) group_idx
  FROM marked_dates
),
replacements AS (
  SELECT orig.subject_id, orig.cohort_date, FIRST_VALUE(cohort_date) OVER (PARTITION BY group_idx ORDER BY ordinal ASC ROWS UNBOUNDED PRECEDING) as replacement_date
  FROM grouped_dates orig
)
SELECT subject_id, cohort_date, replacement_date
INTO #date_replacements
FROM replacements
WHERE cohort_date <> replacement_date;

/*
 * Collapse dates
 */

SELECT
  e.subject_id,
  e.event_cohort_index,
  e.cohort_start_date,
  case
    when e.cohort_start_date = e.cohort_end_date then CAST(dateadd(d,1,e.cohort_end_date) AS DATETIME)
    else e.cohort_end_date
  end cohort_end_date
INTO #coll_dates_events
FROM (
  SELECT
    event.event_cohort_index,
    event.subject_id,
    COALESCE(start_dr.replacement_date, event.cohort_start_date) cohort_start_date,
    COALESCE(end_dr.replacement_date, event.cohort_end_date) cohort_end_date
  FROM #raw_events event
  LEFT JOIN #date_replacements start_dr ON start_dr.subject_id = event.subject_id AND start_dr.cohort_date = event.cohort_start_date
  LEFT JOIN #date_replacements end_dr ON end_dr.subject_id = event.subject_id AND end_dr.cohort_date = event.cohort_end_date
) e
;

-- era-fy the collapsed dates because collapsing leads to overlapping

with cteEndDates (SUBJECT_ID, EVENT_COHORT_INDEX, END_DATE) as
(
	select SUBJECT_ID, EVENT_COHORT_INDEX, EVENT_DATE as END_DATE
	FROM
	(
		select SUBJECT_ID, EVENT_COHORT_INDEX, EVENT_DATE, EVENT_TYPE,
		MAX(START_ORDINAL) OVER (PARTITION BY SUBJECT_ID, EVENT_COHORT_INDEX ORDER BY EVENT_DATE, EVENT_TYPE ROWS UNBOUNDED PRECEDING) as START_ORDINAL,
		ROW_NUMBER() OVER (PARTITION BY SUBJECT_ID, EVENT_COHORT_INDEX ORDER BY EVENT_DATE, EVENT_TYPE) AS OVERALL_ORD
		from
		(
			Select SUBJECT_ID, EVENT_COHORT_INDEX, COHORT_START_DATE AS EVENT_DATE, 1 as EVENT_TYPE, ROW_NUMBER() OVER (PARTITION BY SUBJECT_ID, EVENT_COHORT_INDEX ORDER BY COHORT_START_DATE) as START_ORDINAL
			from #coll_dates_events

			UNION ALL

			select SUBJECT_ID, EVENT_COHORT_INDEX, COHORT_END_DATE, -1 as EVENT_TYPE, NULL
			FROM #coll_dates_events
		) RAWDATA
	) E
	WHERE (2 * E.START_ORDINAL) - E.OVERALL_ORD = 0
)
,cteEpisodeEnds (SUBJECT_ID, EVENT_COHORT_INDEX, COHORT_START_DATE, ERA_END_DATE) as
(
	select
		re.SUBJECT_ID,
		re.EVENT_COHORT_INDEX,
		re.COHORT_START_DATE,
		MIN(ed.END_DATE) as ERA_END_DATE
	FROM #coll_dates_events re
	JOIN cteEndDates ed on re.SUBJECT_ID = ed.SUBJECT_ID and re.EVENT_COHORT_INDEX = ed.EVENT_COHORT_INDEX and ed.END_DATE >= re.COHORT_START_DATE
	GROUP BY
		re.SUBJECT_ID,
		re.EVENT_COHORT_INDEX,
		re.COHORT_START_DATE
)
,cteFinalEras(SUBJECT_ID, EVENT_COHORT_INDEX, COHORT_START_DATE, COHORT_END_DATE) as
(
  select SUBJECT_ID, EVENT_COHORT_INDEX, min(COHORT_START_DATE) as COHORT_START_DATE, ERA_END_DATE as COHORT_END_DATE
	from cteEpisodeEnds e
	group by SUBJECT_ID, EVENT_COHORT_INDEX, ERA_END_DATE
)
select SUBJECT_ID, EVENT_COHORT_INDEX, COHORT_START_DATE, COHORT_END_DATE
INTO #event_cohort_eras
from cteFinalEras;


DROP TABLE IF EXISTS #coll_dates_events;


DROP TABLE IF EXISTS #date_replacements;


DROP TABLE IF EXISTS #raw_events;

-- End Collapse Events
}

/*
 * Split partially overlapping events into non-overlapping segments
 */

WITH
cohort_dates AS (
	SELECT DISTINCT subject_id, cohort_date
	FROM (
		  SELECT subject_id, cohort_start_date cohort_date FROM #event_cohort_eras
		  UNION
		  SELECT subject_id,cohort_end_date cohort_date FROM #event_cohort_eras
		  ) all_dates
),
time_periods AS (
	SELECT subject_id, cohort_date, LEAD(cohort_date,1) over (PARTITION BY subject_id ORDER BY cohort_date ASC) next_cohort_date
	FROM cohort_dates
	GROUP BY subject_id, cohort_date

),
events AS (
	SELECT tp.subject_id, event_cohort_index, cohort_date cohort_start_date, next_cohort_date cohort_end_date
	FROM time_periods tp
	LEFT JOIN #event_cohort_eras e ON e.subject_id = tp.subject_id
	WHERE (e.cohort_start_date <= tp.cohort_date AND e.cohort_end_date >= tp.next_cohort_date)
)
SELECT cast(SUM(POWER(cast(2 as bigint), e.event_cohort_index)) as bigint) as combo_id,  subject_id , cohort_start_date, cohort_end_date
into #combo_events
FROM events e
GROUP BY subject_id, cohort_start_date, cohort_end_date;

/*
 * Remove repetitive events (e.g. A-A-A into A)
 */

SELECT
  CAST(ROW_NUMBER() OVER (PARTITION BY subject_id ORDER BY cohort_start_date) AS INT) ordinal,
  CAST(combo_id AS BIGINT) combo_id,
  subject_id,
  cohort_start_date,
  cohort_end_date
INTO #non_rep_events
FROM (
  SELECT
    combo_id, subject_id, cohort_start_date, cohort_end_date,
    CASE WHEN (combo_id = LAG(combo_id) OVER (PARTITION BY subject_id ORDER BY subject_id, cohort_start_date ASC))
      THEN 1
      ELSE 0
    END repetitive_event,
		case when ROW_NUMBER() OVER (PARTITION BY subject_id, CAST(combo_id AS BIGINT) ORDER BY cohort_start_date) > 1 then 1 else 0 end is_repeat
  FROM #combo_events
) AS marked_repetitive_events
WHERE repetitive_event = 0 {@allow_repeats == 'false'}?{ AND is_repeat = 0 };

/*
 * Persist results
 */

SELECT
  @generation_id as pathway_analysis_generation_id,
  @pathway_target_cohort_id as target_cohort_id,
  subject_id,
	ordinal,
  combo_id,
  cohort_start_date,
  cohort_end_date
INTO #pa_events
FROM #non_rep_events
WHERE 1 = 1 {@max_depth != ''}?{ AND ordinal <= @max_depth };

SELECT
  @generation_id as pathway_analysis_generation_id,
  CAST(@pathway_target_cohort_id AS INT) AS target_cohort_id,
  CAST(target_count.cnt AS BIGINT) AS target_cohort_count,
  CAST(pathway_count.cnt AS BIGINT) AS pathways_count
INTO #pa_stats
FROM (
  SELECT CAST(COUNT_BIG(*) as BIGINT) cnt
  FROM @target_cohort_table
  WHERE cohort_definition_id = @pathway_target_cohort_id
) target_count,
(
  SELECT CAST(COUNT_BIG(DISTINCT subject_id) as BIGINT) cnt
  FROM #pa_events
  WHERE pathway_analysis_generation_id = @generation_id
  AND target_cohort_id = @pathway_target_cohort_id
) pathway_count;

DROP TABLE #non_rep_events;

DROP TABLE #combo_events;


DROP TABLE #event_cohort_eras;

select pathway_analysis_generation_id, target_cohort_id,
	step_1, step_2, step_3, step_4, step_5, step_6, step_7, step_8, step_9, step_10,
  count_big(subject_id) as count_value
INTO #pa_paths
from
(
  select e.pathway_analysis_generation_id, e.target_cohort_id, e.subject_id,
    MAX(case when ordinal = 1 then combo_id end) as step_1,
    MAX(case when ordinal = 2 then combo_id end) as step_2,
    MAX(case when ordinal = 3 then combo_id end) as step_3,
    MAX(case when ordinal = 4 then combo_id end) as step_4,
    MAX(case when ordinal = 5 then combo_id end) as step_5,
    MAX(case when ordinal = 6 then combo_id end) as step_6,
    MAX(case when ordinal = 7 then combo_id end) as step_7,
    MAX(case when ordinal = 8 then combo_id end) as step_8,
    MAX(case when ordinal = 9 then combo_id end) as step_9,
    MAX(case when ordinal = 10 then combo_id end) as step_10
  from #pa_events e
  WHERE e.pathway_analysis_generation_id = @generation_id
	GROUP BY e.pathway_analysis_generation_id, e.target_cohort_id, e.subject_id
) t1
group by pathway_analysis_generation_id, target_cohort_id,
	step_1, step_2, step_3, step_4, step_5, step_6, step_7, step_8, step_9, step_10
;
