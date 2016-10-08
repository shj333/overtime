#!/bin/bash

lein ns-dep-graph 
mv -f ns-dep-graph.png doc/.
open doc/ns-dep-graph.png
