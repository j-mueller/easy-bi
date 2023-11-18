with operators as (select outage_entries.entryoperator_name as name from outage_entries group by outage_entries.entryoperator_name order by count(outage_entries.entryoperator_name) desc limit 10)
select count(*) as count,
       outage_entries.entryoperator_name as operator
from outage_entries
where operator in operators
group by operator
