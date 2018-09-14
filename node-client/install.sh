#!/usr/bin/env bash
BASEPATH=$(cd `dirname $0`; pwd)
RCHAINPATH=$(dirname ${BASEPATH})
mkdir -p ${BASEPATH}/protobuf

# create the Python environment
python3 -m venv ./.virtualenv

.virtualenv/bin/pip install --upgrade pip

# install the Python dependencies
.virtualenv/bin/pip install -r ${BASEPATH}/requirements.txt

# remove all the original protobuf files
rm ${BASEPATH}/protobuf/*.proto
cp ${RCHAINPATH}/models/src/main/protobuf/*.proto ${BASEPATH}/protobuf

Regenerate(){
filename=$(basename $@)
cat $@ | sed 's/import \"scalapb\/scalapb\.proto\"\;//g' | sed 's/\[(scalapb\.field)\..*/;/g' | sed '/^option (scalapb\.options)/,/^};$/s/.*//g'> ${BASEPATH}/protobuf/${filename}
}

# remove the scala code in the protofiles
for file in ${RCHAINPATH}/models/src/main/protobuf/*.proto;
do
Regenerate ${file}
done

for file in ${RCHAINPATH}/comm/src/main/protobuf/coop/rchain/comm/protocol/*.proto
do
Regenerate ${file}
done


# compile the proto files to Python files
.virtualenv/bin/python -m grpc_tools.protoc -I ${BASEPATH}/protobuf --python_out=${BASEPATH} --grpc_python_out=${BASEPATH} ${BASEPATH}/protobuf/*.proto