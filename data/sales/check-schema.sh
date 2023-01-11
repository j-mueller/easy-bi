#! /bin/bash

set -e

cabal build easy-bi-cli

cabal exec easy-bi -- check-types --sql-schema schema.sql --timestamp ORDERDATE