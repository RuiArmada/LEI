import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.AfterEach; 
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * The test class SmartSpeakerTest.
 *
 * @author  grupoPOO 47
 * @version 20220514
 */
public class SmartSpeakerTest 
{
    SmartSpeaker devSpeaker;

    /**
     * Construtor default para a classe de teste SmartSpeakerTest
     */
    public SmartSpeakerTest(){}

    /**
     * Cria uma SmartBulb.
     * Chamado antes de cada método em caso de teste
     */
    @BeforeEach
    public void setUp()
    {
        devSpeaker = new SmartSpeaker(500, "RFM", "JBL", 10, 4.5);
    }

    /**
     * Derruba o dispositivo de teste
     * Chamado após cada método de teste
     */
    @AfterEach
    public void tearDown(){}

    /**
     * Método que testa o construtor vazio da classe SmartSpeaker
     */
    @Test
    public void testSmartSpeakerVazio()
    {
        SmartSpeaker devSpeaker1 = new SmartSpeaker();
        assertEquals(devSpeaker1.getVolume(), 0, "Volume");
        assertEquals(devSpeaker1.getChannel(),"", "Channel");
        assertEquals(devSpeaker1.getBrand(), "", "Brand");
        assertEquals(devSpeaker1.getConsumption(), 0.0, "Consumption");
    }

    /**
     * Método que testa o constructor parametrizado da classe SmartSpeaker
     */
    @Test
    public void testSmartSpeakerParam()
    {
        assertEquals(devSpeaker.getVolume(), 10, "Volume");
        assertEquals(devSpeaker.getChannel(), "RFM", "Channel");
        assertEquals(devSpeaker.getBrand(), "JBL", "Brand");
        assertEquals(devSpeaker.getConsumption(), 4.5, "Consumption");
    }

    /**
     * Método que testa o construtor cópia da classe SmartSpeaker
     */
    @Test
    public void testSmartSpeakerCopia()
    {
        SmartSpeaker devSpeaker2 = new SmartSpeaker(devSpeaker);
        assertEquals(devSpeaker2.getVolume(), 10, "Volume");
        assertEquals(devSpeaker2.getChannel(), "RFM", "Channel");
        assertEquals(devSpeaker2.getBrand(), "JBL", "Brand");
        assertEquals(devSpeaker2.getConsumption(), 4.5, "Consumption");

        assertEquals(devSpeaker, devSpeaker2, "Os objetos são iguais");
    }

    /**
     * Método que testa a devolução do valor correto do volume
     */
    @Test
    public void testGetVolume()
    {
        assertEquals(devSpeaker.getVolume(), 10);
        devSpeaker = new SmartSpeaker(500, "RFM", "JBL", SmartSpeaker.MAX, 4.5);
        assertEquals(devSpeaker.getVolume(), 20);
        devSpeaker = new SmartSpeaker(500, "RFM", "JBL", -10, 4.5);
        assertEquals(devSpeaker.getVolume(), 0);
        devSpeaker = new SmartSpeaker();
        assertEquals(devSpeaker.getVolume(), 0);
    }

    /**
     * Método que testa a devolução do canal
     * */
    @Test
    public void testGetChannel()
    {
        assertEquals(devSpeaker.getChannel(), "RFM");
        devSpeaker = new SmartSpeaker();
        assertEquals(devSpeaker.getChannel(), "");
    }

    /**
     * Método que testa a devolução da marca
     */
    @Test
    public void testGetBrand()
    {
        assertEquals(devSpeaker.getBrand(), "JBL");
        devSpeaker = new SmartSpeaker();
        assertEquals(devSpeaker.getBrand(), "");
    }

    /**
     * Método que testa a devolução do consumo
     */
    @Test
    public void testGetConsumption()
    {
        assertEquals(devSpeaker.getConsumption(), 4.5);
        devSpeaker = new SmartSpeaker(500, "RFM", "JBL", 10, -4.5);
        assertEquals(devSpeaker.getConsumption(), 0);
        devSpeaker = new SmartSpeaker();
        assertEquals(devSpeaker.getConsumption(), 0);
    }

    /**
     * Método que testa a atualização do volume
     */
    @Test
    public void testSetVolume()
    {
        devSpeaker.volumeUp();
        devSpeaker.volumeUp();
        assertEquals(12, devSpeaker.getVolume());
        for (int i=0; i<20; i++) devSpeaker.volumeUp();
        assertEquals(20, devSpeaker.getVolume());
        for (int i=0; i<20; i++) devSpeaker.volumeDown();
        assertEquals(0, devSpeaker.getVolume());
    }

    /**
     * Método que testa a atualização do canal
     */
    @Test
    public void testSetChannel()
    {
        devSpeaker.setChannel("Megahits");
        assertEquals("Megahits", devSpeaker.getChannel());
    }

    /**
     * Método que testa a atualização da marca
     */
    @Test
    public void testSetBrand()
    {
        devSpeaker.setBrand("Sony");
        assertEquals("Sony", devSpeaker.getBrand());
    }

    /**
     * Método que testa a atualização do consumo
     */
    @Test
    public void testSetConsumption()
    {
        devSpeaker.setConsumption(5.1);
        assertEquals(5.1, devSpeaker.getConsumption());
        devSpeaker.setConsumption(0);
        assertEquals(0, devSpeaker.getConsumption());
    }
}
