package UMAirlines.src;

/**
 * Contains a message with extra informations.
 * Using a frame, we are able to send messsages that contain info about the type of message
 * it is and the user who sent it.
 */
public class Frames {
    
    public final int tag;
    public final String name;
    public final byte[] data;

    /**
     * Creates a new <code>Frames</code> containing some information.
     * @param tag Frame's type
     * @param name User's, who sent the message, username
     * @param data the message contained in the frame
     */
    public Frames(int tag, String name, byte[] data) {
        this.tag = tag;
        this.name = name;
        this.data = data;
    }

}
