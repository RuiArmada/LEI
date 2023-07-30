import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.AfterEach; 
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * The test class CasaInteligenteTest.
 *
 * @author  grupoPOO 47
 * @version 20220514
 */
public class CasaInteligenteTest 
{
    CasaInteligente house;

    /**
     * Construtor default para a classe de teste CasaInteligenteTest
     */
    public CasaInteligenteTest(){}

    /**
     * Cria uma Casa Inteligente.
     * Chamado antes de cada método em caso de teste
     */
    @BeforeEach
    public void setUp()
    {
        house = new CasaInteligente("José", 500, "EDP");
    }

    /**
     * Derruba o dispositivo de teste
     * Chamado após cada método de teste
     */
    @AfterEach
    public void tearDown(){}

     /**
     * Método que testa o construtor vazio da classe CasaInteligente
     */
    @Test
    public void testCasaInteligenteVazio()
    {
        CasaInteligente house1 = new CasaInteligente();
        assertEquals(house1.getOwnerName(), "", "Name");
        assertEquals(house1.getOwnerNIF(), 0, "NIF");
        assertEquals(house1.getSupplier(), "", "Supplier");
    }

     /**
     * Método que testa o constructor parametrizado da classe CasaInteligente
     */
    @Test
    public void testCasaInteligenteParam()
    {
        assertEquals(house.getOwnerName(), "José", "Name");
        assertEquals(house.getOwnerNIF(), 500, "NIF");
        assertEquals(house.getSupplier(), "EDP", "Supplier");
    }

    /**
     * Método que testa o construtor cópia da classe CasaInteligente
     */
    @Test
    public void testCasaInteligenteCopia()
    {
        CasaInteligente house2 = new CasaInteligente(house);
        assertEquals(house2.getOwnerName(), "José", "Name");
        assertEquals(house2.getOwnerNIF(), 500, "NIF");
        assertEquals(house2.getSupplier(), "EDP", "Supplier");

        assertEquals(house, house2, "Os objetos são iguais");
    }

    /**
     * Método que testa a devoluçao do NIF do proprietário da Casa
     */
    @Test
    void testGetOwnerNIF() 
    {
        assertEquals(house.getOwnerNIF(), 500);
        house = new CasaInteligente("José", -2, "EDP");
        assertEquals(house.getOwnerNIF(), 0);
        house = new CasaInteligente();
        assertEquals(house.getOwnerNIF(), 0);
    }

    /**
     * Método que testa a devolução do nome do proprietário da Casa
     */
    @Test
    void testGetOwnerName() 
    {
        assertEquals(house.getOwnerName(), "José");
        house = new CasaInteligente();
        assertEquals(house.getOwnerName(), "");
    }

    /**
     * Método que testa a devolução do nome do fornecedor associado à Casa
     */
    @Test
    void testGetSupplier() 
    {
        assertEquals(house.getSupplier(), "EDP");
        house = new CasaInteligente();
        assertEquals(house.getSupplier(), "");
    }

    /**
     * Método que testa a atualização do NIF do proprietário da Casa
     */
    @Test
    void testSetOwnerNIF() 
    {
        house.setOwnerNIF(300);
        assertEquals(300, house.getOwnerNIF());
        house.setOwnerNIF(0);
        assertEquals(0, house.getOwnerNIF());
    }

    /**
     * Método que testa a atualização do nome do proprietário da Casa
     */
    @Test
    void testSetOwnerName() 
    {
        house.setOwnerName("Maria");
        assertEquals("Maria", house.getOwnerName());
    }

    /**
     * Método que testa a atualização do nome do fornecedor associado à Casa
     */
    @Test
    void testSetSupplier() 
    {
        house.setSupplier("Galp");
        assertEquals("Galp", house.getSupplier());
    }

    /**
     * Método que testa a inserção de uma divisão na casa
     */
    @Test
    void testAddRoom() 
    {
        house.addRoom("sala");
        assertTrue(house.hasRoom("sala"));
        assertFalse(house.hasRoom("quarto"));
    }

    /**
     * Método que testa a inserção de dispositivos em divisões da casa
     */
    @Test
    void testAddToRoom() 
    {
        SmartDevice devBulb = new SmartBulb(100);
        SmartDevice devBulb1 = new SmartBulb(101);
        SmartDevice devBulb2 = new SmartBulb(102);
        house.addRoom("sala");
        house.addRoom("quarto");
        house.addSmartDevice(100, "sala",devBulb);
        house.addSmartDevice(101,"sala",devBulb1);
        house.addSmartDevice(102,"quarto",devBulb2);
        assertTrue(house.roomHasDevice("sala", 100));
        assertTrue(house.roomHasDevice("sala", 101));
        assertFalse(house.roomHasDevice("sala", 102));
        assertTrue(house.roomHasDevice("quarto", 102));
    }

    /**
     * Método que testa se existe o dispositivo na casa
     */
    @Test
    void testGetDevice() 
    {
        SmartDevice devBulb = new SmartBulb(100);
        house.addRoom("sala");
        house.addSmartDevice(100, "sala",devBulb);
        assertTrue(house.getDevice(100).equals(devBulb));

    }

    /**
     * Método que testa se todos os dispositivos são ligados
     */
    @Test
    void testSetAllOn() 
    {
        SmartDevice devBulb = new SmartBulb(100);
        SmartDevice devBulb1 = new SmartBulb(101);
        house.addRoom("sala");
        house.addSmartDevice(100, "sala",devBulb);
        house.addSmartDevice(101, "sala",devBulb1);
        assertTrue(house.getDevice(100).getOn());
        assertFalse(house.getDevice(101).getOn());
        house.setAllOn(true);
        assertTrue(house.getDevice(100).getOn());
        assertTrue(house.getDevice(101).getOn());
        house.setAllOn(false);
        assertFalse(house.getDevice(100).getOn());
        assertFalse(house.getDevice(101).getOn());
    }
}
