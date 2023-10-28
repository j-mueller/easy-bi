select json_object('operator', operator, 'count', count)
from (with view_0 as (select outage_entries.entryoperator_name
                      from outage_entries
                      group by outage_entries.entryoperator_name
                      order by COUNT * desc
                      limit 10)
      select outage_entries.entryoperator_name as operator,
             COUNT(*) as count
      from outage_entries
      where outage_entries.entryoperator_name in view_0
      group by outage_entries.entryoperator_name)