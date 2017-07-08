#!/usr/bin/env bash
cd /Users/kent/Documents/Rholang/
sbt 'run-main coop.rchain.rho2rose.Rholang2RosetteCompiler /Users/kent/Documents/RholangREPL/test.rho' > ~/Documents/RholangREPL/output.txt
# Navneet's script
cp ~/Documents/RholangREPL/output.txt ~/Documents/RholangREPL/original_output.txt
sed -n '/\[0m\[\[32msuccess\[0m\]/!p' ~/Documents/RholangREPL/output.txt > ~/Documents/RholangREPL/output2.txt
sed -n '/\[0m\[\[0minfo\[0m\]/!p' ~/Documents/RholangREPL/output2.txt > ~/Documents/RholangREPL/output3.txt
cat ~/Documents/RholangREPL/output3.txt