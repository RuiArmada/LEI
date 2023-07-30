import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.AfterEach; 
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * The test class InvoiceTest
 *
 * @author  grupoPOO 47
 * @version 20220514
 */
public class InvoiceTest 
{
    Invoice invoice;

    /**
     * Construtor default para a classe de teste InvoiceTest
     */
    public InvoiceTest(){}

    /**
     * Cria uma fatura (invoice).
     * Chamado antes de cada método em caso de teste
     */
    @BeforeEach
    public void setUp()
    {
        invoice = new Invoice("EDP", 500, 20, 25.6, 320.34);
    }

    /**
     * Derruba o dispositivo de teste
     * Chamado após cada método de teste
     */
    @AfterEach
    public void tearDown(){}

    /**
     * Método que testa o construtor vazio da classe Invoice
     */
    @Test
    public void testInvoiceVazio()
    {
        Invoice invoice1 = new Invoice();
        assertEquals(invoice1.getSupplier(), "", "Supplier");
        assertEquals(invoice1.getNif(), 0, "NIF");
        assertEquals(invoice1.getDays(), 0, "Days");
        assertEquals(invoice1.getTotalConsumption(), 0.0, "TotalConsumption");
        assertEquals(invoice1.getTotalPrice(), 0.0, "TotalPrice");
    }

     /**
     * Método que testa o constructor parametrizado da classe Invoice
     */
    @Test
    public void testInvoiceParam()
    {
        assertEquals(invoice.getSupplier(), "EDP", "Supplier");
        assertEquals(invoice.getNif(), 500, "NIF");
        assertEquals(invoice.getDays(), 20, "Days");
        assertEquals(invoice.getTotalConsumption(), 25.6, "TotalConsumption");
        assertEquals(invoice.getTotalPrice(), 320.34, "TotalPrice");
    }

    /**
     * Método que testa o construtor cópia da classe Invoice
     */
    @Test
    public void testInvoiceCopia()
    {
        Invoice invoice2 = new Invoice(invoice);
        assertEquals(invoice2.getSupplier(), "EDP", "Supplier");
        assertEquals(invoice2.getNif(), 500, "NIF");
        assertEquals(invoice2.getDays(), 20, "Days");
        assertEquals(invoice2.getTotalConsumption(), 25.6, "TotalConsumption");
        assertEquals(invoice2.getTotalPrice(), 320.34, "TotalPrice");

        assertEquals(invoice, invoice2, "Os objetos são iguais");
    }

    /**
     * Método que testa a devolução dos dias da Invoice
     */
    @Test
    void testGetDays() 
    {
        assertEquals(invoice.getDays(), 20);
        invoice = new Invoice("EDP", 500, -2, 25.6, 320.34);
        assertEquals(invoice.getDays(), 0);
        invoice = new Invoice();
        assertEquals(invoice.getDays(), 0);
    }

    /**
     * Método que testa a devolução do NIF do proprietario referente à Invoice
     */
    @Test
    void testGetNif() 
    {
        assertEquals(invoice.getNif(), 500);
        invoice = new Invoice("EDP", -2, 20, 25.6, 320.34);
        assertEquals(invoice.getNif(), 0);
        invoice = new Invoice();
        assertEquals(invoice.getNif(), 0);
    }

    /**
     * Método que testa a devolução do fornecedor referente à Invoice
     */
    @Test
    void testGetSupplier() 
    {
        assertEquals(invoice.getSupplier(), "EDP");
        invoice = new Invoice();
        assertEquals(invoice.getSupplier(), "");
    }

    /**
     * Método que testa a devolução do consumo total da invoice
     */
    @Test
    void testGetTotalConsumption() 
    {
        assertEquals(invoice.getTotalConsumption(), 25.6);
        invoice = new Invoice("EDP", 500, 20, -2.6, 320.34);
        assertEquals(invoice.getTotalConsumption(), 0.0);
        invoice = new Invoice();
        assertEquals(invoice.getTotalConsumption(), 0.0);
    }

    /**
     * Método que testa a devolução do preço total da invoice
     */
    @Test
    void testGetTotalPrice() 
    {
        assertEquals(invoice.getTotalPrice(), 320.34);
        invoice = new Invoice("EDP", 500, 20, 25.6, -3.34);
        assertEquals(invoice.getTotalPrice(), 0.0);
        invoice = new Invoice();
        assertEquals(invoice.getTotalPrice(), 0.0);
    }

    /**
     * Método que testa a atualização do número de dias da invoice
     */
    @Test
    void testSetDays() 
    {
        invoice.setDays(30);
        assertEquals(30, invoice.getDays());
        invoice.setDays(0);
        assertEquals(0, invoice.getDays());
    }

    /**
     * Método que testa a atualização do NIF do proprietário referente na invoice
     */
    @Test
    void testSetNif() 
    {
        invoice.setNif(300);
        assertEquals(300, invoice.getNif());
        invoice.setNif(0);
        assertEquals(0, invoice.getNif());
    }

    /**
     * Método que testa a atualização do fornecedor da invoice
     */
    @Test
    void testSetSupplier() 
    {
        invoice.setSupplier("Galp");
        assertEquals("Galp", invoice.getSupplier());
    }

    /**
     * Método que testa a atualização do consumo total da invoice
     */
    @Test
    void testSetTotalConsumption() 
    {
        invoice.setTotalConsumption(10.5);
        assertEquals(10.5, invoice.getTotalConsumption());
        invoice.setTotalConsumption(0.0);
        assertEquals(0.0, invoice.getTotalConsumption());
    }

    /**
     * Método que testa a atualização do preço total da invoice
     */
    @Test
    void testSetTotalPrice() 
    {
        invoice.setTotalPrice(100.5);
        assertEquals(100.5, invoice.getTotalPrice());
        invoice.setTotalPrice(0.0);
        assertEquals(0.0, invoice.getTotalPrice());
    }
}
