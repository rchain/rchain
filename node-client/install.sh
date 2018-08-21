#!/usr/bin/env bash
BASEPATH=$(cd `dirname $0`; pwd)
RCHAINPATH=$(dirname ${BASEPATH})

echo ${BASEPATH}
# install the python dependencies
pip install -r ${BASEPATH}/requirements.txt

# remove all the original protobuf files
rm -f ${BASEPATH}/protobuf/*.proto
cp ${RCHAINPATH}/models/src/main/protobuf/*.proto ${BASEPATH}/protobuf

Regerate(){
filename=$(basename $@)
cat $@ | sed 's/import \"scalapb\/scalapb\.proto\"\;//g' | sed 's/\[(scalapb\.field)\..*/;/g' | sed '/^option (scalapb\.options)/,/^};$/s/.*//g'> ${BASEPATH}/protobuf/${filename}
}

# remove the scala code in the protofiles
for file in ${RCHAINPATH}/models/src/main/protobuf/*.proto;
do
Regerate ${file}
done

for file in ${RCHAINPATH}/comm/src/main/protobuf/coop/rchain/comm/protocol/*.proto
do
Regerate ${file}
done


# compile the proto files to python files
python -m grpc_tools.protoc -I ${BASEPATH}/protobuf --python_out=${BASEPATH} --grpc_python_out=${BASEPATH} ${BASEPATH}/protobuf/*.proto