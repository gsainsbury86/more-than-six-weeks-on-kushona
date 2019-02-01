SELECT
   week,
   min(date),
   max(date),
   min(date) + (max(date) - min(date)) / 2 * interval '1 day' as average,
   COALESCE( ( 
   SELECT
      min(date) + (max(date) - min(date)) / 2 * interval '1 day' as average 
   FROM
      adventure adv_2 
   WHERE
      adv_1.week < adv_2.week 
   GROUP BY
      week 
   ORDER BY
      week limit 1), '2019-01-29') - min(date) + (max(date) - min(date)) / 2 * interval '1 day' as days_to_next_session 
   FROM
      adventure adv_1 
   GROUP BY
      week 
   ORDER BY
      week