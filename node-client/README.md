# RChain node client

To run a contract (assuming the RChain node is running):

    python RChain.py contract1.rho

To execute a rholang process:

    python RChain.py -c 'new x in { x!(1 + 1) }'

See RChain.py for more details.

## Troubleshooting

### problem: ImportError: No module named google.protobuf

solution:

```
pip install -r requirements.txt
```

### problem: StatusCode.UNAVAILABLE

```
grpc._channel._Rendezvous: <_Rendezvous of RPC that terminated with (StatusCode.UNAVAILABLE, Connect Failed)>
```

solution: match port number in `insecure_channel` with server log:

```
Server started, listening on 50000
```


### problem: OPENSSL_internal:WRONG_VERSION_NUMBER

`E0412 23:52:32.670525498   26190 ssl_transport_security.cc:989] Handshake failed with fatal error SSL_ERROR_SSL: error:100000f7:SSL routines:OPENSSL_internal:WRONG_VERSION_NUMBER.`

solution: don't use `grpc.secure_channel`
