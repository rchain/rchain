rm -rf KeyValueStore *.class

rm -rf testdb

scalac -classpath /home/griff/.ivy2/cache/org.lmdbjava/lmdbjava/jars/lmdbjava-0.0.2.jar KeyValueStore.scala

scalac -classpath /home/griff/.ivy2/cache/org.lmdbjava/lmdbjava/jars/lmdbjava-0.0.2.jar:/home/griff/Documents/sprint_1/lmdbjava_trans -sourcepath /home/griff/Documents/sprint_1/lmdbjava_trans KvsLmdbTest.scala

echo 'next:'
echo 'scala -classpath /home/griff/.ivy2/cache/org.lmdbjava/lmdbjava/jars/lmdbjava-0.0.2.jar:/home/griff/.ivy2/cache/com.github.jnr/jnr-ffi/jars/jnr-ffi-2.0.9.jar:/home/griff/.ivy2/cache/com.github.jnr/jffi/jars/jffi-1.2.11.jar:/home/griff/.ivy2/cache/com.github.jnr/jffi/jars/jffi-1.2.11-native.jar:/home/griff/.ivy2/cache/org.ow2.asm/asm/jars/asm-5.0.3.jar:/home/griff/.ivy2/cache/org.ow2.asm/asm-commons/jars/asm-commons-5.0.3.jar:/home/griff/.ivy2/cache/org.ow2.asm/asm-tree/jars/asm-tree-5.0.3.jar:/home/griff/.ivy2/cache/org.ow2.asm/asm-analysis/jars/asm-analysis-5.0.3.jar:/home/griff/.ivy2/cache/org.ow2.asm/asm-util/jars/asm-util-5.0.3.jar:/home/griff/.ivy2/cache/com.github.jnr/jnr-x86asm/jars/jnr-x86asm-1.0.2.jar:/home/griff/.ivy2/cache/com.github.jnr/jnr-constants/jars/jnr-constants-0.9.2.jar:/home/griff/.ivy2/cache/com.jakewharton.byteunits/byteunits/jars/byteunits-0.9.1.jar:/home/griff/.ivy2/cache/org.deephacks.lmdbjni/lmdbjni-linux64/jars/lmdbjni-linux64-0.4.7.jar:/home/griff/.ivy2/cache/org.deephacks.lmdbjni/lmdbjni/jars/lmdbjni-0.4.7.jar:. KvsLmdbTest'

