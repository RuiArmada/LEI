import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.AfterEach; 
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import java.util.HashMap;
import java.util.Map;

/**
 * The test class SmartSpeakerTest.
 *
 * @author  grupoPOO 47
 * @version 20220514
 */
public class SuppliersTest 
{
    Suppliers suppliers;
    Map<String, EnergySupplier> allSuppliers;
    EnergySupplier sup1, sup2;
    
    /**
     * Constructor default para a classe SuppliersTest
     */
    public SuppliersTest(){}

    /**
     * Define o conjunto de fornecedores de energia
     * Chamado antes de cada método de caso de teste
     */
    @BeforeEach
    public void setUp()
    {
        allSuppliers = new HashMap<>();

        sup1 = new EnergySupplier("EDP", 4.5);
        sup2 = new EnergySupplier("Galp", 2.3);

        allSuppliers.put("EDP", sup1);
        allSuppliers.put("Galp", sup2);

        suppliers = new Suppliers(allSuppliers);
    }

    /**
     * Derruba o dispositivo de teste
     * Chamado após cada método de teste
     */
    @AfterEach
    public void tearDown(){}

    /**
     * Método que testa o construtor vazio da classe Suppliers
     */
    @Test
    public void testSuppliersVazio()
    {
        suppliers = new Suppliers();

        assertEquals(suppliers.getSuppliers(), new HashMap<String, EnergySupplier>());
    }
    
    /**
     * Método que testa o construtor de parametrização da classe Suppliers
     */
    @Test
    public void testSuppliersParam()
    {
        assertEquals(suppliers.getSuppliers(), allSuppliers);
    }

    /**
     * Método que testa o constructor de cópia da classe Suppliers
     */
    @Test
    public void testSuppliersCopia()
    {
        Suppliers suppliers2 = new Suppliers(suppliers);

        assertEquals(suppliers2.getSuppliers(), allSuppliers);
    }

    /**
     * Método que testa a devolução de todos os fornecedores da classe Suppliers
     */
    @Test
    void testGetSuppliers() 
    {
        assertEquals(suppliers.getSuppliers(), allSuppliers);
    }

    @Test
    void testSetSuppliers() 
    {
        Map<String, EnergySupplier> allSuppliers2 = new HashMap<>();

        EnergySupplier sup1 = new EnergySupplier("Enat", 4.0);
        EnergySupplier sup2 = new EnergySupplier("Endesa", 1.4);

        allSuppliers2.put("EDP", sup1);
        allSuppliers2.put("Galp", sup2);

        suppliers = new Suppliers(allSuppliers2);

        assertEquals(suppliers.getSuppliers(), allSuppliers2);
    }

    /**
     * Método que testa se, consoante o nome, é devolvido o fornecedor incluido na classe Suppliers
     */
    @Test
    void testGetSupplier() 
    {
        assertEquals(suppliers.getSupplier("EDP"), sup1);
    }
}
