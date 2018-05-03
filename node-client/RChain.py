'''RChain node client

Usage:
  python RChain.py contract1.rho
  python RChain.py -c 'new x in { x!(1 + 1) }'

We assume the RChain node is running and that it is listening on port
5000. Double-check that you see this message in the logs:

  Server started, listening on 50000

The output should be something like:

Storage Contents:
 @{15a23988-03df-4835-9c55-fb9fbf843a47}!(2) |
 for( x0, x1 <= @{\"stdoutAck\"} ) { Nil } |
 for( x0 <= @{\"stdout\"} ) { Nil } |
 for( x0, x1 <= @{\"stderrAck\"} ) { Nil } |
 for( x0 <= @{\"stderr\"} ) { Nil }"

Tested with rnode-assembly.jar from commit 8a357eed5dd Apr 12 2018.

'''

from __future__ import print_function

# cribbed from https://grpc.io/docs/tutorials/basic/python.html
import repl_pb2
import repl_pb2_grpc


def main(argv, stdout, insecure_channel,
         host='127.0.0.1',
         port=50000):
    channel = insecure_channel('%s:%s' % (host, port))
    replCh = repl_pb2_grpc.ReplStub(channel)

    if '-c' in argv:
        line = argv[-1]
        req = repl_pb2.CmdRequest(line=line)
        output = replCh.Run(req).output
    else:
        fileName = argv[1]
        req = repl_pb2.EvalRequest(fileName=fileName)
        output = replCh.Eval(req).output
    print(output, file=stdout)


if __name__ == '__main__':
    def _script():
        from sys import argv, stdout

        from grpc import insecure_channel

        main(argv, stdout, insecure_channel)
    _script()
