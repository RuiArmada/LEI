package smart_houses.Testes;

import org.junit.jupiter.api.Test;
import smart_houses.Fatura;
import smart_houses.exceptions.AlreadyExistDeviceException;
import smart_houses.exceptions.FornecedorErradoException;
import smart_houses.modulo_casas.Casa;
import smart_houses.modulo_fornecedores.Fornecedor;
import smart_houses.smart_devices.SmartBulb;
import smart_houses.smart_devices.SmartDevice;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class FornecedorTest {

    @Test
    void criaFatura() throws AlreadyExistDeviceException, FornecedorErradoException {
        Casa c = new Casa("Artur", "256", "EDP");
        Fornecedor f = new Fornecedor("EDP");
        SmartDevice sm = new SmartBulb(true, 0.20, SmartBulb.Tones.WARM, 10);
        SmartDevice sm2 = new SmartBulb(true, 0.20, SmartBulb.Tones.WARM, 10);
        c.addDevice(sm);
        c.addDevice(sm2);
        Fatura fatura = f.criaFatura(c, LocalDate.now(), LocalDate.now().plusDays(10));
        assertNotNull(fatura);
        assertEquals(fatura.getInicioPeriodo(), LocalDate.now());
        assertEquals(fatura.getFimPeriodo(), LocalDate.now().plusDays(10));
        assertEquals("EDP", fatura.getFornecedor());
        assertEquals("256", fatura.getNifCliente());
        assertEquals(10 * (sm.comsumption() + sm2.comsumption()), fatura.getConsumo());
        assertEquals(0.64395, fatura.getCusto());
    }

    @Test
    void faturacao() throws FornecedorErradoException, AlreadyExistDeviceException {
        Casa c = new Casa("Artur", "256", "EDP");
        Fornecedor f = new Fornecedor("EDP");
        SmartDevice sm = new SmartBulb(true, 0.20, SmartBulb.Tones.WARM, 10);
        SmartDevice sm2 = new SmartBulb(true, 0.20, SmartBulb.Tones.WARM, 10);
        c.addDevice(sm);
        c.addDevice(sm2);
        Fatura fatura = f.criaFatura(c, LocalDate.now(), LocalDate.now().plusDays(10));
        Fatura fatura2 = f.criaFatura(c, LocalDate.now(), LocalDate.now().plusDays(10));
        f.adicionaFatura(fatura2);
        f.adicionaFatura(fatura);
        assertNotNull(fatura);
        assertEquals(fatura.getInicioPeriodo(), LocalDate.now());
        assertEquals(fatura.getFimPeriodo(), LocalDate.now().plusDays(10));
        assertEquals(0.64395 * 2, f.faturacao());
    }

    @Test
    void fornecedorTest1() {
        Fornecedor f = new Fornecedor();
        assertEquals("n/a", f.getName());
        assertEquals(0.1, f.getDesconto());
        assertTrue(f.getFaturas().isEmpty());
    }

    @Test
    void fornecedorTest2() {
        Fornecedor f = new Fornecedor("EDP", 0.1);
        assertEquals("EDP", f.getName());
        assertEquals(0.1, f.getDesconto());
        assertTrue(f.getFaturas().isEmpty());
    }

    @Test
    void copyTest() {
        Fornecedor f = new Fornecedor("EDP", 0.1);
        Fornecedor cf = new Fornecedor(f);
        assertEquals(f.getName(), cf.getName());
        assertEquals(f.getDesconto(), cf.getDesconto());
        assertEquals(f.getFaturas(), cf.getFaturas());
    }

    @Test
    void getSetTest() {
        Fornecedor f = new Fornecedor("EDP", 0.1);
        f.setName("Electric Company");
        f.setDesconto(0.2);
        assertEquals("Electric Company", f.getName());
        assertEquals(0.2, f.getDesconto());
    }

    @Test
    void setFaturaTest() {
        Fornecedor f = new Fornecedor("EDP", 0.1);
        List<Fatura> faturas = new ArrayList<>();
        faturas.add(new Fatura("EDP", "256", 100.0, 50.0, LocalDate.now(), LocalDate.now().plusDays(5)));
        f.setFaturas(faturas);

        List<Fatura> fornecedorFaturas = f.getFaturas();
        assertEquals(faturas.size(), fornecedorFaturas.size());
        for (int i = 0; i < faturas.size(); i++) {
            assertNotSame(faturas.get(i), fornecedorFaturas.get(i));
            assertEquals(faturas.get(i), fornecedorFaturas.get(i));
        }
    }
    @Test
    void testCriaFatura() throws FornecedorErradoException {
        Fornecedor f = new Fornecedor("EDP", 0.1);
        Casa c = new Casa("Artur", "256", "EDP");
        LocalDate inicio = LocalDate.now();
        LocalDate fim = LocalDate.now().plusDays(10);

        Fatura fatura = f.criaFatura(c, inicio, fim);
        assertNotNull(fatura);
        assertEquals("EDP", fatura.getFornecedor());
        assertEquals("256", fatura.getNifCliente());
        assertEquals(inicio, fatura.getInicioPeriodo());
        assertEquals(fim, fatura.getFimPeriodo());
        assertEquals(0.0, fatura.getConsumo());
        assertEquals(0.0, fatura.getCusto());
    }
    @Test
    void testCriaFaturaThrowsException() {
        Fornecedor f = new Fornecedor("EDP", 0.1);
        Casa c = new Casa("Artur", "256", "EDP");
        Casa c1 = new Casa("John", "123", "Different Supplier");

        assertThrows(FornecedorErradoException.class, () ->
                f.criaFatura(c1, LocalDate.now(), LocalDate.now().plusDays(10))
        );
    }
    @Test
    void testEqualsAndHashCode() {
        Fornecedor f = new Fornecedor("EDP", 0.1);
        Fornecedor f1= new Fornecedor(f);
        Fornecedor f2 = new Fornecedor("Different");
        assertEquals(f, f);
        assertEquals(f, f1);
        assertNotEquals(f, f2);
        assertEquals(f.hashCode(), f1.hashCode());
        assertNotEquals(f.hashCode(), f2.hashCode());
    }
    }
