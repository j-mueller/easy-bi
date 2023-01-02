#! /bin/bash

set -e

cabal build easy-bi-cli

cabal exec easy-bi -- --sql-schema schema.sql --timestamp ORDERDATE