# RNode windows build
## Important
During windows build you may see the following error: "java.util.regex.PatternSyntaxException: Unclosed group near index 9". This is an issue inside scalamft formatter, https://github.com/scalameta/sbt-scalafmt/issues/5
To fix it just disable scalafmt: find "scalafmtOnCompile := true" line in the "build.sbt" file, and replace "true" with "false".

## 1. Install Haskell Platform
https://www.haskell.org/platform/
	make sure that ghc compiler and cabal are available from PATH

## 2. Download and build BNFC
https://github.com/BNFC/bnfc
	Read BNFC build instructions for details.

## 3. Install cup
http://www2.cs.tum.edu/projects/cup/install.php

## 4. Install JFLex
http://jflex.de/download.html
	
	update JFLEX_HOME in jflex.bat
	Make “jflex.bat” available from PATH (add <location>\jflex\bin to PATH variable)

## 5. Open sbt shell and generate parser with bnfc
```
bnfc:clean
bnfc:generate
```
## 6. From sbt shell build node

You can follow the rnode build instruction (see https://github.com/rchain/rchain/blob/master/node/README.md), but on windows it might be easier to work with stage build, with all jars in the same folder buy running:
```
node/universal:stage
```
from the sbt shell.

The build artifacts will be located in the node/target/universal/stage directory if the build is successful.

## 7. Prepare certificates, keys, etc.
Follow official user guide:
https://rchain.atlassian.net/wiki/spaces/CORE/pages/428376065/User+guide+for+running+RNode#UserguideforrunningRNode-Bootstrapnode

## 8. Prepare a command line or a .toml file
See also [rnode.toml](rnode.toml) example

## 9. Start RNode
For easy start you can use rnode.bat start file generated during build. For example:
```
rnode.bat run --data-dir C:\RChain\data
```

Sometimes the following errors could appear "invalid syntax" and "the command is too long" or "the path is too long". If you see those errors, open rnode.bat and find the very long line (usally 83) that starts with 
```
set "APP_CLASSPATH=...<<< many .jar files >>>" 
```
with:
```
set "APP_CLASSPATH=%APP_LIB_DIR%\*"
```
