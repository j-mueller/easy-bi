#! /bin/bash

cabal run easy-bi-cli -- start-server \
    --sql-schema=src/server/test/data/schema.sql \
    --sqlite-db=src/server/test/data/sales.db
