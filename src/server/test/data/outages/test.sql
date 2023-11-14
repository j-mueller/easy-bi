select strftime('%d',
                      outage_entries.entrydate_start) as outage_day_of_month,
             outage_entries.entry_duration_minutes as duration
      from outage_entries