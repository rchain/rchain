package org.zeromq;

import java.io.IOException;
import java.nio.channels.Selector;
import java.util.Arrays;

import org.zeromq.ZMQ.Socket;

/**
 * First implementation of an agent for a remotely controlled background service for 0MQ.
 * Used in conjunction with a ZStar, but not mandatory.
 *<p>
 * An agent is a mechanism allowing to send messages from one thread to another, and to receive messages from this other thread.
 *<p>
 * Its built-in implementation provides an easy communication-lock
 * system that will close the access once the remote thread is finished.
 *<p>
 * Are proposed for you a restrained set of simple but powerful messaging commands for a quick learning curve
 * and an access to the underlying Socket for advanced usage.
 */
// agent for a remote controlled background message processing API for 0MQ.
// contract to be agent of a star
public interface ZAgent
{
    /**
     * Receives a control message sent from the Plateau in the Corbeille.
     * The call is blocking.
     *
     * @return the received message or null if the context was shut down.
     */
    ZMsg recv();

    /**
     * Receives a control message sent from the Plateau in the Corbeille.
     * The call is blocking depending on the parameter.
     *
     * @param wait   true to make a blocking call, false to not wait, and possibly return null
     * @return the received message or null if the context was shut down or if no message if not blocking.
     */
    ZMsg recv(boolean wait);

    /**
     * Sends a control message from the Corbeille to the Plateau.
     *
     * @param message    the message to send
     * @return true if the message was sent, otherwise false (if the distant Star is dead for example)
     */
    boolean send(ZMsg message);

    /**
     * Sends a control message from Corbeille side to the Plateau side.
     *
     * @param msg       the message to send
     * @param destroy   true to destroy the message after sending it.
     * @return true if the message was sent, otherwise false (if the distant Star is dead for example)
     */
    boolean send(ZMsg msg, boolean destroy);

    /**
     * Sends a control message from the Corbeille to the Plateau side.
     *
     * @param word    the message to send
     * @return true if the message was sent, otherwise false (if the distant Star is dead for example)
     */
    boolean send(String word);

    /**
     * Sends a control message from the Corbeille to the Plateau side.
     *
     * @param word    the message to send
     * @param more   true to send more strings in a single message
     * @return true if the message was sent, otherwise false (if the distant Star is dead for example)
     */
    boolean send(String word, boolean more);

    /**
     * Gives a sign if the distant Star is here.
     *
     * @return true if here, otherwise false
     */
    boolean sign();

    /**
     * Forcely destroys the Star.
     * @deprecated not sure it is useful or recommended
     */
    @Deprecated
    void nova();

    /**
     * Returns the socket used for communication.
     * For advanced usage.
     *
     * @return the socket used to communicate with the distant Star.
     */
    Socket pipe();

    public static class Creator
    {
        public static ZAgent create(Socket pipe, String lock)
        {
            return new SimpleAgent(pipe, lock);
        }
    }

    /**
     * Creates a very simple agent with an easy lock mechanism.
     */
    public static final class SimpleAgent implements ZAgent
    {
        // the pipe used for communicating with the star
        private final Socket pipe;

        // the key used to lock the agent.
        private final byte[] lock;

        // the locked state.
        private boolean locked;

        /**
         * Creates a new simple agent.
         *
         * @param pipe   the pipe used to send control messages to the distant IStar.
         * @param lock   the lock to use. If null, the locking mechanism is omitted.
         */
        public SimpleAgent(Socket pipe, String lock)
        {
            this.pipe = pipe;
            this.lock = lock == null ? null : lock.getBytes(ZMQ.CHARSET);
        }

        @Override
        public boolean sign()
        {
            return !locked;
        }

        @Override
        public ZMsg recv()
        {
            return recv(true);
        }

        @Override
        public ZMsg recv(boolean wait)
        {
            if (locked) {
                return null;
            }
            try {
                ZMsg msg = ZMsg.recvMsg(pipe, wait ? 0 : ZMQ.DONTWAIT);
                if (msg == null) {
                    return null;
                }

                final ZFrame frame = msg.peek();
                byte[] key = frame.getData();
                if (lock != null && Arrays.equals(lock, key)) {
                    locked = true;
                    // this is the last message anyway, and not a one for a public display
                    msg = null;
                    pipe.close();
                }
                return msg;
            }
            catch (ZMQException e) {
                locked = true;
                return null;
            }
        }

        @Override
        public boolean send(ZMsg message)
        {
            if (locked) {
                return false;
            }
            return message.send(pipe);
        }

        @Override
        public boolean send(String word)
        {
            if (locked) {
                return false;
            }
            return pipe.send(word);
        }

        @Override
        public boolean send(String word, boolean more)
        {
            if (locked) {
                return false;
            }
            return pipe.send(word, more ? ZMQ.SNDMORE : 0);
        }

        @Override
        public boolean send(ZMsg msg, boolean destroy)
        {
            if (locked) {
                return false;
            }
            return msg.send(pipe, destroy);
        }

        @Override
        public Socket pipe()
        {
            return pipe;
        }

        @Override
        public void nova()
        {
            pipe.close();
        }
    }

    /**
     * Creates a selector and destroys it.
     */
    // Contract for selector creation.
    // will be called in backstage side.
    public static interface SelectorCreator
    {
        /**
         * Creates and opens a selector.
         *
         * @return the opened selector.
         */
        Selector create() throws IOException;

        /**
         * Destroys the previously opened selector.
         * @param selector the selector to close
         */
        void destroy(Selector selector) throws IOException;
    }

    // very simple selector creator
    public static class VerySimpleSelectorCreator implements SelectorCreator
    {
        @Override
        public Selector create() throws IOException
        {
            return Selector.open();
        }

        @Override
        public void destroy(Selector selector) throws IOException
        {
            if (selector != null) {
                selector.close();
            }
        }
    }
}
