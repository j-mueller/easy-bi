CREATE TABLE outage_entries
    ( entry_id INTEGER NOT NULL
    , entry_operator_id INTEGER NOT NULL
    , entry_subsection VARCHAR
    , entry_type INTEGER NOT NULL
    , entryorigin INTEGER NOT NULL
    , entrygeo_type INTEGER NOT NULL
    , entrydate_start TIMESTAMP WITH TIME ZONE NOT NULL
    , entrydate_end TIMESTAMP WITH TIME ZONE NOT NULL
    , entry_duration_minutes INTEGER NOT NULL
    , entrylast_update VARCHAR NOT NULL
    , entrypostal_code VARCHAR NOT NULL
    , entrycity VARCHAR NOT NULL
    , entrydistrict VARCHAR
    , entrystreet VARCHAR
    , entryradius DOUBLE PRECISION
    , entry_coordinate_system_type VARCHAR NOT NULL
    , entrycoordinates VARCHAR
    , entrylive_info VARCHAR
    , entrycomments VARCHAR
    , entrydefinition VARCHAR
    , entrysocial VARCHAR
    , entrysocial_text VARCHAR
    , entryis_outage_in_area BOOLEAN NOT NULL
    , entryis_fixed BOOLEAN NOT NULL
    , entry_sector_type INTEGER NOT NULL
    , entryoperator_name VARCHAR NOT NULL
    , PRIMARY KEY(entry_id));

CREATE VIEW "outages" AS
    SELECT
        strftime('%d', outage_entries.entrydate_start) as outage_day_of_month
        , strftime('%m', outage_entries.entrydate_start) as outage_month
        , strftime('%H', outage_entries.entrydate_start) as outage_hour
        , date(outage_entries.entrydate_start) as outage_date
        , outage_entries.entryoperator_name as operator
        , outage_entries.entrycity as city
        , SUM(outage_entries.entry_duration_minutes) as duration_sum
        , AVG(outage_entries.entry_duration_minutes) as duration_avg
        , COUNT(*) as count
    FROM outage_entries
