import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.AfterEach; 
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * The test class SmartBulbTest
 *
 * @author  grupoPOO 47
 * @version 20220514
 */
public class SmartBulbTest 
{
    SmartBulb devBulb;

     /**
     * Construtor default para a classe de teste SmartBulbTest
     */
    public SmartBulbTest(){}

    /**
     * Cria uma SmartBulb.
     * Chamado antes de cada método em caso de teste
     */
    @BeforeEach
    public void setUp()
    {
        devBulb = new SmartBulb(500, 2, 10.2, 4.5);
    }

    /**
     * Derruba o dispositivo de teste
     * Chamado após cada método de teste
     */
    @AfterEach
    public void tearDown(){}

    /**
     * Método que testa o construtor vazio da classe SmartBulb
     */
    @Test
    public void testSmartBulbVazio()
    {
        SmartBulb devBulb1 = new SmartBulb();
        assertEquals(devBulb1.getTone(), 1, "Tone");
        assertEquals(devBulb1.getDimension(), 0, "Dimension");
        assertEquals(devBulb1.getConsumption(), 0.0, "Consumption");
    }

     /**
     * Método que testa o constructor parametrizado da classe SmartBulb
     */
    @Test
    public void testSmartBulbParam()
    {
        assertEquals(devBulb.getTone(), 2, "Tone");
        assertEquals(devBulb.getDimension(), 10.2, "Dimension");
        assertEquals(devBulb.getConsumption(), 4.5, "Consumption");
    }

    /**
     * Método que testa o construtor cópia da classe SmartBulb
     */
    @Test
    public void testSmartBulbCopia()
    {
        SmartBulb devBulb2 = new SmartBulb(devBulb);
        assertEquals(devBulb2.getTone(), 2, "Tone");
        assertEquals(devBulb2.getDimension(), 10.2, "Dimension");
        assertEquals(devBulb2.getConsumption(), 4.5, "Consumption");

        assertEquals(devBulb, devBulb2, "Os objetos são iguais");
    }

    /**
     * Método que testa a devolução do consumo da SmartBulb
    */
    @Test
    void testGetConsumption() 
    {
        assertEquals(devBulb.getConsumption(), 4.5);
        devBulb = new SmartBulb(500, 2, 10.2, -4.5);
        assertEquals(devBulb.getConsumption(), 0);
        devBulb = new SmartBulb();
        assertEquals(devBulb.getConsumption(), 0);
    }

    /**
     * Método que testa a devolução da dimensão da SmartBulb
     */
    @Test
    void testGetDimension() 
    {
        assertEquals(devBulb.getDimension(), 10.2);
        devBulb = new SmartBulb(500, 2, -2.0, 4.5);
        assertEquals(devBulb.getDimension(), 0);
        devBulb = new SmartBulb();
        assertEquals(devBulb.getDimension(), 0);
    }

    /**
     * Método que testa a devolução da intensidade da SmartBulb
     */
    @Test
    void testGetTone() 
    {
        assertEquals(devBulb.getTone(), 2);
        devBulb = new SmartBulb(500, -4, 10.2, 4.5);
        assertEquals(devBulb.getTone(), 0);
        devBulb = new SmartBulb(500, 5, 10.2, 4.5);
        assertEquals(devBulb.getTone(), 2);
        devBulb = new SmartBulb();
        assertEquals(devBulb.getTone(), 1);

    }

    /**
     * Método que testa a atualização do consumo da SmartBulb
     */
    @Test
    void testSetConsumption() 
    {
        devBulb.setConsumption(5.1);
        assertEquals(5.1, devBulb.getConsumption());
        devBulb.setConsumption(0);
        assertEquals(0, devBulb.getConsumption());
    }

    /**
     * Método que testa a atualização da dimensão da SmartBulb
     */
    @Test
    void testSetDimension() 
    {
        devBulb.setDimension(10.5);
        assertEquals(10.5, devBulb.getDimension());
        devBulb.setDimension(0);
        assertEquals(0, devBulb.getDimension());
    }

    /**
     * Método que testa a atualização da intensidade da SmartBulb
     */
    @Test
    void testSetTone() 
    {
        devBulb.setTone(1);
        assertEquals(1, devBulb.getTone());
        devBulb.setTone(0);
        assertEquals(0, devBulb.getTone());
    }
}
