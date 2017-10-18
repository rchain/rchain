package zmq;

import java.util.HashSet;
import java.util.Set;

class SessionBase extends Own implements
                        Pipe.IPipeEvents, IPollEvents,
                        IMsgSink, IMsgSource
{
    //  If true, this session (re)connects to the peer. Otherwise, it's
    //  a transient session created by the listener.
    private boolean connect;

    //  Pipe connecting the session to its socket.
    private Pipe pipe;

    //  This set is added to with pipes we are disconnecting, but haven't yet completed
    private final Set<Pipe> terminatingPipes;

    //  This flag is true if the remainder of the message being processed
    //  is still in the in pipe.
    private boolean incompleteIn;

    //  True if termination have been suspended to push the pending
    //  messages to the network.
    private boolean pending;

    //  The protocol I/O engine connected to the session.
    private IEngine engine;

    //  The socket the session belongs to.
    protected SocketBase socket;

    //  I/O thread the session is living in. It will be used to plug in
    //  the engines into the same thread.
    private IOThread ioThread;

    //  ID of the linger timer
    private static final int LINGER_TIMER_ID = 0x20;

    //  True is linger timer is running.
    private boolean hasLingerTimer;

    //  If true, identity has been sent/received from the network.
    private boolean identitySent;
    private boolean identityReceived;

    //  Protocol and address to use when connecting.
    private final Address addr;

    private IOObject ioObject;

    public static SessionBase create(IOThread ioThread, boolean connect,
            SocketBase socket, Options options, Address addr)
    {
        SessionBase s = null;
        switch (options.type) {
        case ZMQ.ZMQ_REQ:
            s = new  Req.ReqSession(ioThread, connect,
                socket, options, addr);
            break;
        case ZMQ.ZMQ_DEALER:
            s = new Dealer.DealerSession(ioThread, connect,
                socket, options, addr);
            break;
        case ZMQ.ZMQ_REP:
            s = new Rep.RepSession(ioThread, connect,
                socket, options, addr);
            break;
        case ZMQ.ZMQ_ROUTER:
            s = new Router.RouterSession(ioThread, connect,
                socket, options, addr);
            break;
        case ZMQ.ZMQ_PUB:
            s = new Pub.PubSession(ioThread, connect,
                socket, options, addr);
            break;
        case ZMQ.ZMQ_XPUB:
            s = new XPub.XPubSession(ioThread, connect,
                socket, options, addr);
            break;
        case ZMQ.ZMQ_SUB:
            s = new  Sub.SubSession(ioThread, connect,
                socket, options, addr);
            break;
        case ZMQ.ZMQ_XSUB:
            s = new XSub.XSubSession(ioThread, connect,
                socket, options, addr);
            break;
        case ZMQ.ZMQ_PUSH:
            s = new Push.PushSession(ioThread, connect,
                socket, options, addr);
            break;
        case ZMQ.ZMQ_PULL:
            s = new Pull.PullSession(ioThread, connect,
                socket, options, addr);
            break;
        case ZMQ.ZMQ_PAIR:
            s = new Pair.PairSession(ioThread, connect,
                socket, options, addr);
            break;
        default:
            throw new IllegalArgumentException("type=" + options.type);
        }
        return s;
    }

    public SessionBase(IOThread ioThread, boolean connect,
            SocketBase socket, Options options, Address addr)
    {
        super(ioThread, options);
        ioObject = new IOObject(ioThread);

        this.connect = connect;
        pipe = null;
        incompleteIn = false;
        pending = false;
        engine = null;
        this.socket = socket;
        this.ioThread = ioThread;
        hasLingerTimer = false;
        identitySent = false;
        identityReceived = false;
        this.addr = addr;

        terminatingPipes = new HashSet<Pipe>();
    }

    @Override
    public void destroy()
    {
        assert (pipe == null);

        //  If there's still a pending linger timer, remove it.
        if (hasLingerTimer) {
            ioObject.cancelTimer(LINGER_TIMER_ID);
            hasLingerTimer = false;
        }

        //  Close the engine.
        if (engine != null) {
            engine.terminate();
        }
    }

    //  To be used once only, when creating the session.
    public void attachPipe(Pipe pipe)
    {
        assert (!isTerminating());
        assert (this.pipe == null);
        assert (pipe != null);
        this.pipe = pipe;
        this.pipe.setEventSink(this);
    }

    public Msg pullMsg()
    {
        //  First message to send is identity
        if (!identitySent) {
            Msg msg = new Msg(options.identitySize);
            msg.put(options.identity, 0, options.identitySize);
            identitySent = true;
            incompleteIn = false;

            return msg;
        }

        if (pipe == null) {
            return null;
        }

        Msg msg = pipe.read();
        if (msg == null) {
            return null;
        }
        incompleteIn = msg.hasMore();

        return msg;

    }

    @Override
    public int pushMsg(Msg msg)
    {
        //  First message to receive is identity (if required).
        if (!identityReceived) {
            msg.setFlags(Msg.IDENTITY);
            identityReceived = true;

            if (!options.recvIdentity) {
                return 0;
            }
        }

        if (pipe != null && pipe.write(msg)) {
            return 0;
        }

        return ZError.EAGAIN;
    }

    protected void reset()
    {
        //  Restore identity flags.
        identitySent = false;
        identityReceived = false;
    }

    public void flush()
    {
        if (pipe != null) {
            pipe.flush();
        }
    }

    //  Remove any half processed messages. Flush unflushed messages.
    //  Call this function when engine disconnect to get rid of leftovers.
    private void cleanPipes()
    {
        if (pipe != null) {
            //  Get rid of half-processed messages in the out pipe. Flush any
            //  unflushed messages upstream.
            pipe.rollback();
            pipe.flush();

            //  Remove any half-read message from the in pipe.
            while (incompleteIn) {
                Msg msg = pullMsg();
                if (msg == null) {
                    assert (!incompleteIn);
                    break;
                }
                // msg.close ();
            }
        }
    }

    @Override
    public void pipeTerminated(Pipe pipe)
    {
        //  Drop the reference to the deallocated pipe.
        assert (this.pipe == pipe || terminatingPipes.contains(pipe));

        if (this.pipe == pipe) {
            // If this is our current pipe, remove it
            this.pipe = null;
            if (hasLingerTimer) {
                ioObject.cancelTimer(LINGER_TIMER_ID);
                hasLingerTimer = false;
            }
        }
        else {
            // Remove the pipe from the detached pipes set
            terminatingPipes.remove(pipe);
        }

        //  If we are waiting for pending messages to be sent, at this point
        //  we are sure that there will be no more messages and we can proceed
        //  with termination safely.
        if (pending && this.pipe == null && terminatingPipes.isEmpty()) {
            pending = false;
            super.processTerm(0);
        }
    }

    @Override
    public void readActivated(Pipe pipe)
    {
        // Skip activating if we're detaching this pipe
        if (this.pipe != pipe) {
            assert (terminatingPipes.contains(pipe));
            return;
        }

        if (engine != null) {
            engine.activateOut();
        }
        else {
            this.pipe.checkRead();
        }
    }

    @Override
    public void writeActivated(Pipe pipe)
    {
        // Skip activating if we're detaching this pipe
        if (this.pipe != pipe) {
            assert (terminatingPipes.contains(pipe));
            return;
        }

        if (engine != null) {
            engine.activateIn();
        }
    }

    @Override
    public void hiccuped(Pipe pipe)
    {
        //  Hiccups are always sent from session to socket, not the other
        //  way round.
        throw new UnsupportedOperationException("Must Override");

    }

    public SocketBase getSocket()
    {
        return socket;
    }

    @Override
    protected void processPlug()
    {
        ioObject.setHandler(this);
        if (connect) {
            startConnecting(false);
        }
    }

    @Override
    protected void processAttach(IEngine engine)
    {
        assert (engine != null);

        //  Create the pipe if it does not exist yet.
        if (pipe == null && !isTerminating()) {
            ZObject[] parents = {this, socket};
            Pipe[] pipes = {null, null};
            int[] hwms = {options.recvHwm, options.sendHwm};
            boolean[] delays = {options.delayOnClose, options.delayOnDisconnect};
            Pipe.pipepair(parents, pipes, hwms, delays);

            //  Plug the local end of the pipe.
            pipes[0].setEventSink(this);

            //  Remember the local end of the pipe.
            assert (pipe == null);
            pipe = pipes[0];

            //  Ask socket to plug into the remote end of the pipe.
            sendBind(socket, pipes[1]);
        }

        //  Plug in the engine.
        assert (this.engine == null);
        this.engine = engine;
        this.engine.plug(ioThread, this);
    }

    public void detach()
    {
        //  Engine is dead. Let's forget about it.
        engine = null;

        //  Remove any half-done messages from the pipes.
        cleanPipes();

        //  Send the event to the derived class.
        detached();

        //  Just in case there's only a delimiter in the pipe.
        if (pipe != null) {
            pipe.checkRead();
        }
    }

    protected void processTerm(int linger)
    {
        assert (!pending);

        //  If the termination of the pipe happens before the term command is
        //  delivered there's nothing much to do. We can proceed with the
        //  stadard termination immediately.
        if (pipe == null && terminatingPipes.isEmpty()) {
            super.processTerm(0);
            return;
        }

        pending = true;

        //  If there's finite linger value, delay the termination.
        //  If linger is infinite (negative) we don't even have to set
        //  the timer.
        if (linger > 0) {
            assert (!hasLingerTimer);
            ioObject.addTimer(linger, LINGER_TIMER_ID);
            hasLingerTimer = true;
        }

        //  Start pipe termination process. Delay the termination till all messages
        //  are processed in case the linger time is non-zero.
        if (pipe != null) {
            pipe.terminate(linger != 0);

            //  TODO: Should this go into pipe_t::terminate ?
            //  In case there's no engine and there's only delimiter in the
            //  pipe it wouldn't be ever read. Thus we check for it explicitly.
            pipe.checkRead();
        }
    }

    @Override
    public void timerEvent(int id)
    {
        //  Linger period expired. We can proceed with termination even though
        //  there are still pending messages to be sent.
        assert (id == LINGER_TIMER_ID);
        hasLingerTimer = false;

        //  Ask pipe to terminate even though there may be pending messages in it.
        assert (pipe != null);
        pipe.terminate(false);
    }

    private void detached()
    {
        //  Transient session self-destructs after peer disconnects.
        if (!connect) {
            terminate();
            return;
        }

        //  For delayed connect situations, terminate the pipe
        //  and reestablish later on
        if (pipe != null && options.delayAttachOnConnect == 1
            && !addr.protocol().equals("pgm") && !addr.protocol().equals("epgm")) {
            pipe.hiccup();
            pipe.terminate(false);
            terminatingPipes.add(pipe);
            pipe = null;
        }

        reset();

        //  Reconnect.
        if (options.reconnectIvl != -1) {
            startConnecting(true);
        }

        //  For subscriber sockets we hiccup the inbound pipe, which will cause
        //  the socket object to resend all the subscriptions.
        if (pipe != null && (options.type == ZMQ.ZMQ_SUB || options.type == ZMQ.ZMQ_XSUB)) {
            pipe.hiccup();
        }
    }

    private void startConnecting(boolean wait)
    {
        assert (connect);

        //  Choose I/O thread to run connecter in. Given that we are already
        //  running in an I/O thread, there must be at least one available.
        IOThread ioThread = chooseIoThread(options.affinity);
        assert (ioThread != null);

        //  Create the connecter object.

        if (addr.protocol().equals("tcp")) {
            TcpConnecter connecter = new TcpConnecter(
                ioThread, this, options, addr, wait);
            launchChild(connecter);
            return;
        }

        if (addr.protocol().equals("ipc")) {
            IpcConnecter connecter = new IpcConnecter(
                ioThread, this, options, addr, wait);
            launchChild(connecter);
            return;
        }

        assert (false);
    }

    @Override
    public String toString()
    {
        return super.toString() + "[" + options.socketId + "]";
    }

    @Override
    public void inEvent()
    {
        throw new UnsupportedOperationException();
    }

    @Override
    public void outEvent()
    {
        throw new UnsupportedOperationException();
    }

    @Override
    public void connectEvent()
    {
        throw new UnsupportedOperationException();
    }

    @Override
    public void acceptEvent()
    {
        throw new UnsupportedOperationException();
    }
}
