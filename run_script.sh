#!/bin/sh

script="$1"
goal="$2"

echo "[lost], ['$script'], $goal"
prism -g "[lost], ['$script'], $goal"

