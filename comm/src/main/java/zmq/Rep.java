package zmq;

public class Rep extends Router
{
    public static class RepSession extends Router.RouterSession
    {
        public RepSession(IOThread ioThread, boolean connect,
            SocketBase socket, final Options options,
            final Address addr)
        {
            super(ioThread, connect, socket, options, addr);
        }
    }
    //  If true, we are in process of sending the reply. If false we are
    //  in process of receiving a request.
    private boolean sendingReply;

    //  If true, we are starting to receive a request. The beginning
    //  of the request is the backtrace stack.
    private boolean requestBegins;

    public Rep(Ctx parent, int tid, int sid)
    {
        super(parent, tid, sid);
        sendingReply = false;
        requestBegins = true;

        options.type = ZMQ.ZMQ_REP;
    }

    @Override
    protected boolean xsend(Msg msg)
    {
        //  If we are in the middle of receiving a request, we cannot send reply.
        if (!sendingReply) {
            throw new IllegalStateException("Cannot send another reply");
        }

        boolean more = msg.hasMore();

        //  Push message to the reply pipe.
        boolean rc = super.xsend(msg);
        if (!rc) {
            return rc;
        }

        //  If the reply is complete flip the FSM back to request receiving state.
        if (!more) {
            sendingReply = false;
        }

        return true;
    }

    @Override
    protected Msg xrecv()
    {
        //  If we are in middle of sending a reply, we cannot receive next request.
        if (sendingReply) {
            throw new IllegalStateException("Cannot receive another request");
        }

        Msg msg = null;
        //  First thing to do when receiving a request is to copy all the labels
        //  to the reply pipe.
        if (requestBegins) {
            while (true) {
                msg = super.xrecv();
                if (msg == null) {
                    return null;
                }

                if (msg.hasMore()) {
                    //  Empty message part delimits the traceback stack.
                    boolean bottom = (msg.size() == 0);

                    //  Push it to the reply pipe.
                    boolean rc = super.xsend(msg);
                    assert (rc);
                    if (bottom) {
                        break;
                    }
                }
                else {
                    //  If the traceback stack is malformed, discard anything
                    //  already sent to pipe (we're at end of invalid message).
                    super.rollback();
                }
            }
            requestBegins = false;
        }

        //  Get next message part to return to the user.
        msg = super.xrecv();
        if (msg == null) {
            return null;
        }

        //  If whole request is read, flip the FSM to reply-sending state.
        if (!msg.hasMore()) {
            sendingReply = true;
            requestBegins = true;
        }

        return msg;
    }

    @Override
    protected boolean xhasIn()
    {
        return !sendingReply && super.xhasIn();
    }

    @Override
    protected boolean xhasOut()
    {
        return sendingReply && super.xhasOut();
    }
}
