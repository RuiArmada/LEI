import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.AfterEach; 
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * The test class EnergySupplierTest
 *
 * @author  grupoPOO 47
 * @version 20220514
 */
public class EnergySupplierTest 
{
    EnergySupplier energSup;

    /**
     * Construtor default para a classe de teste EnergySupplier
     */
    public EnergySupplierTest(){}

    /**
     * Cria um Energy Supplier.
     * Chamado antes de cada método em caso de teste
     */
    @BeforeEach
    public void setUp()
    {
        energSup = new EnergySupplier("EDP", 4.5);
    }

    /**
     * Derruba o dispositivo de teste
     * Chamado após cada método de teste
     */
    @AfterEach
    public void tearDown(){}

    /**
     * Método que testa o construtor vazio da classe EnergySupplier
     */
    @Test
    public void testEnergySupplierVazio()
    {
        EnergySupplier energSup1 = new EnergySupplier();
        assertEquals(energSup1.getName(), "", "Name");
        assertEquals(energSup1.getPrice(), 0.0, "Price");
    }

     /**
     * Método que testa o constructor parametrizado da classe EnergySupplier
     */
    @Test
    public void testEnergySupplierParam()
    {
        assertEquals(energSup.getName(), "EDP", "Name");
        assertEquals(energSup.getPrice(), 4.5, "Price");
    }

    /**
     * Método que testa o construtor cópia da classe EnergySupplier
     */
    @Test
    public void testEnergySupplierCopia()
    {
        EnergySupplier energSup2 = new EnergySupplier(energSup);
        assertEquals(energSup2.getName(), "EDP", "Name");
        assertEquals(energSup2.getPrice(), 4.5, "Price");
        assertEquals(energSup2.getTaxes(), 0.23, "Taxes");

        assertEquals(energSup, energSup2, "Os objetos são iguais");
    }

    /**
     * Método que testa a devolução do nome do fornecedor
     */
    @Test
    void testGetName() 
    {
        assertEquals(energSup.getName(), "EDP");
        energSup = new EnergySupplier();
        assertEquals(energSup.getName(), "");
    }

    /**
     * Método que testa a devolução do preço praticado pelo fornecedor
     */
    @Test
    void testGetPrice() 
    {
        assertEquals(energSup.getPrice(), 4.5);
        energSup = new EnergySupplier("EDP", -4.5);
        assertEquals(energSup.getPrice(), 0.0);
        energSup = new EnergySupplier();
        assertEquals(energSup.getPrice(), 0.0);
    }

    /**
     * Método que testa a atualização do nome do fornecedor
     */
    @Test
    void testSetName() 
    {
        energSup.setName("Galp");
        assertEquals("Galp", energSup.getName());
    }

    /**
     * Método que testa a atualização do preço praticado pelo fornecedor
     */
    @Test
    void testSetPrice() 
    {
        energSup.setPrice(5.1);
        assertEquals(5.1, energSup.getPrice());
        energSup.setPrice(0.0);
        assertEquals(0.0, energSup.getPrice());

    }   
}
