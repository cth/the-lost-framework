#!/bin/sh

script="$1"
goal="$2"

echo "[lost], ['$script'], run($goal)."
prism -g "[lost], ['$script'], run($goal)."

