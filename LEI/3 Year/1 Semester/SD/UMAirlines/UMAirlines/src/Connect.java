package UMAirlines.src;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.Socket;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Class that handles the connections in the application.
 * To be more specific, it handles the flux of frames in the app.
 */
public class Connect implements AutoCloseable {

    private final DataInputStream in;
    private final DataOutputStream ou;
    private final Lock xl = new ReentrantLock(); // Read mode Lock
    private final Lock yl = new ReentrantLock(); // Write mode Lock
    
    public Connect(Socket socket) throws IOException {
        this.in = new DataInputStream(new BufferedInputStream(socket.getInputStream()));
        this.ou = new DataOutputStream(new BufferedOutputStream(socket.getOutputStream()));
    }

    /**
     * Sends a frame
     * @param f Frame
     * @throws IOException
     */
    public void send_frame(Frames f) throws IOException {
        try {
            yl.lock();
            this.ou.writeInt(f.tag);
            this.ou.writeUTF(f.name);
            this.ou.writeInt(f.data.length);
            this.ou.write(f.data);
            this.ou.flush();
        } finally {
            yl.unlock();
        }
    }

    /**
     * Send a frame by givint it's basic components
     * @param tag Frame's id tag
     * @param name Name of the User that sent the Frame
     * @param data Information contained in the Frame
     * @throws IOException
     */
    public void send_f_Info(int tag, String name, byte[] data) throws IOException {
        this.send_frame(new Frames(tag, name, data));
    }

    /**
     * Produces a new frame
     * @return New Frame
     * @throws IOException
     */
    public Frames recieve() throws IOException {
        int tag;
        String name;
        byte[] data;
        try {
            xl.lock();
            tag = this.in.readInt();
            name = this.in.readUTF();
            int n = this.in.readInt();
            data = new byte[n];
            this.in.readFully(data);
        } finally {
            xl.unlock();
        }
        return new Frames(tag, name, data);
    }

    /**
     * Closes the intput and output streams
     */
    @Override
    public void close() throws IOException {
        this.in.close();
        this.ou.close();
    }
    
}
