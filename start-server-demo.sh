#! /bin/bash

set -e

cabal build easy-bi-cli

cabal exec easy-bi -- start-server \
    --sql-schema=src/server/test/data/outages/schema.sql \
    --data-source-config=src/server/test/data/outages/datasource.yaml \
    --sqlite-db=src/server/test/data/outages/outages.sqlite \
    --port=8080
