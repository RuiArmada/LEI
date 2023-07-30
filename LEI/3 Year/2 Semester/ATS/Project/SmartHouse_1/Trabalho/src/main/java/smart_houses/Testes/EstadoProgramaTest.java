package smart_houses.Testes;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import smart_houses.EstadoPrograma;
import smart_houses.Fatura;
import smart_houses.SerializableConsumer;
import smart_houses.exceptions.*;
import smart_houses.modulo_casas.Casa;
import smart_houses.modulo_fornecedores.Fornecedor;
import smart_houses.smart_devices.SmartBulb;
import smart_houses.smart_devices.SmartCamera;
import smart_houses.smart_devices.SmartDevice;
import smart_houses.smart_devices.SmartSpeaker;

import java.time.LocalDate;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;

class EstadoProgramaTest {
    @Test
    void getCasaMaisGastadora1() throws FornecedorInexistenteException, ExisteCasaException, ExisteFornecedorException {
        EstadoPrograma e = new EstadoPrograma();
        Optional<Casa> casaMaisGastadora = e.getCasaMaisGastadora();
        assertTrue(casaMaisGastadora.isEmpty());
    }

    @Test
    void GetCasaMaisGastadora2() throws ExisteFornecedorException, FornecedorInexistenteException, ExisteCasaException {
        EstadoPrograma e = new EstadoPrograma();
        Fornecedor f = new Fornecedor("EDP");
        Casa c1 = new Casa("Artur", "250", "EDP");
        Casa c2 = new Casa("Afonso", "251", "EDP");
        Casa c3 = new Casa("Goncalo", "252", "EDP");

        Fatura f1 = new Fatura("EDP", "250", 120, 100, LocalDate.now(), LocalDate.now().plusDays(10));
        Fatura f2 = new Fatura("EDP", "250", 180, 100, LocalDate.now(), LocalDate.now().plusDays(10));
        Fatura f3 = new Fatura("EDP", "250", 190, 100, LocalDate.now(), LocalDate.now().plusDays(10));
        Fatura f4 = new Fatura("EDP", "250", 80, 100, LocalDate.now(), LocalDate.now().plusDays(10));
        Fatura f5 = new Fatura("EDP", "250", 150, 100, LocalDate.now(), LocalDate.now().plusDays(10));

        c1.adicionaFatura(f1);
        c1.adicionaFatura(f2);
        c2.adicionaFatura(f3);
        c2.adicionaFatura(f5);
        c3.adicionaFatura(f4);

        e.addFornecedor(f);
        e.adicionaCasa(c1);
        e.adicionaCasa(c2);
        e.adicionaCasa(c3);

        Optional<Casa> casaMaisGastadora = e.getCasaMaisGastadora();
        assertTrue(casaMaisGastadora.isPresent());
        assertEquals(c2, casaMaisGastadora.get());
    }

    @Test
    void maiorConsumidorPeriodo1() {
        EstadoPrograma e = new EstadoPrograma();
        List<Casa> maiorConsumidores = e.maiorConsumidorPeriodo(LocalDate.of(2022, 1, 1), LocalDate.of(2022, 12, 31), 2);
        assertTrue(maiorConsumidores.isEmpty());
    }

    @Test
    void maiorConsumidorPeriodo2() throws ExisteFornecedorException, FornecedorInexistenteException, ExisteCasaException {
        EstadoPrograma e = new EstadoPrograma();
        Fornecedor f = new Fornecedor("EDP");
        Casa c1 = new Casa("Artur", "250", "EDP");
        Casa c2 = new Casa("Afonso", "251", "EDP");
        Casa c3 = new Casa("Goncalo", "252", "EDP");

        Fatura f1 = new Fatura("EDP", "250", 120, 100, LocalDate.now(), LocalDate.now().plusDays(10));
        Fatura f2 = new Fatura("EDP", "250", 180, 100, LocalDate.now(), LocalDate.now().plusDays(10));
        Fatura f3 = new Fatura("EDP", "250", 190, 100, LocalDate.now(), LocalDate.now().plusDays(10));
        Fatura f4 = new Fatura("EDP", "250", 80, 100, LocalDate.now(), LocalDate.now().plusDays(10));
        Fatura f5 = new Fatura("EDP", "250", 150, 100, LocalDate.now(), LocalDate.now().plusDays(10));

        c1.adicionaFatura(f1);
        c1.adicionaFatura(f2);
        c2.adicionaFatura(f3);
        c2.adicionaFatura(f5);
        c3.adicionaFatura(f4);

        e.addFornecedor(f);
        e.adicionaCasa(c1);
        e.adicionaCasa(c2);
        e.adicionaCasa(c3);

        List<Casa> maiorConsumidores = e.maiorConsumidorPeriodo(LocalDate.of(2022, 1, 1), LocalDate.of(2022, 12, 31), 2);
        assertFalse(maiorConsumidores.isEmpty());
    }

    @Test
    void maiorConsumidorPeriodo3() throws FornecedorInexistenteException, ExisteCasaException, ExisteFornecedorException {
        EstadoPrograma e= new EstadoPrograma();
        Fornecedor fornecedor = new Fornecedor("EDP");


        Casa casa1 = new Casa("Artur", "250", "EDP");
        Casa casa2 = new Casa("Afonso", "251", "EDP");
        Casa casa3 = new Casa("Goncalo", "252", "EDP");

        Fatura fatura1 = new Fatura("EDP", "250", 120, 100, LocalDate.of(2022, 1, 1), LocalDate.of(2022, 2, 1));
        Fatura fatura2 = new Fatura("EDP", "250", 180, 100, LocalDate.of(2022, 2, 1), LocalDate.of(2022, 3, 1));
        Fatura fatura3 = new Fatura("EDP", "250", 190, 100, LocalDate.of(2022, 3, 1), LocalDate.of(2022, 4, 1));
        Fatura fatura4 = new Fatura("EDP", "250", 80, 100, LocalDate.of(2022, 1, 1), LocalDate.of(2022, 2, 1));
        Fatura fatura5 = new Fatura("EDP", "250", 150, 100, LocalDate.of(2021, 1, 1), LocalDate.of(2021, 12, 31));

        casa1.adicionaFatura(fatura1);
        casa1.adicionaFatura(fatura2);
        casa2.adicionaFatura(fatura3);
        casa2.adicionaFatura(fatura5);
        casa3.adicionaFatura(fatura4);

        e.addFornecedor(fornecedor);
        e.adicionaCasa(casa1);
        e.adicionaCasa(casa2);
        e.adicionaCasa(casa3);

        List<Casa> maiorConsumidores = e.maiorConsumidorPeriodo(LocalDate.of(2022, 1, 1), LocalDate.of(2022, 12, 31), 2);
        assertEquals(2, maiorConsumidores.size());
        assertTrue(maiorConsumidores.contains(casa2));
        assertTrue(maiorConsumidores.contains(casa1));
        }
    @Test
    void equalsTest1() throws FornecedorInexistenteException, ExisteCasaException, ExisteFornecedorException {
        EstadoPrograma estado1 = new EstadoPrograma();
        EstadoPrograma estado2 = new EstadoPrograma();

        Fornecedor fornecedor1 = new Fornecedor("EDP");
        Fornecedor fornecedor2 = new Fornecedor("Endesa");
        estado1.addFornecedor(fornecedor1);
        estado1.addFornecedor(fornecedor2);
        estado2.addFornecedor(fornecedor1);
        estado2.addFornecedor(fornecedor2);

        Casa casa1 = new Casa("Artur", "250", "EDP");
        Casa casa2 = new Casa("Afonso", "251", "EDP");
        estado1.adicionaCasa(casa1);
        estado1.adicionaCasa(casa2);
        estado2.adicionaCasa(casa1);
        estado2.adicionaCasa(casa2);

        assertTrue(estado1.equals(estado2));
    }
    @Test
    void equalsTest2() throws FornecedorInexistenteException, ExisteCasaException, ExisteFornecedorException {
        EstadoPrograma estado1 = new EstadoPrograma();
        EstadoPrograma estado2 = new EstadoPrograma();

        Fornecedor fornecedor1 = new Fornecedor("EDP");
        Fornecedor fornecedor2 = new Fornecedor("Endesa");
        estado1.addFornecedor(fornecedor1);
        estado1.addFornecedor(fornecedor2);
        estado2.addFornecedor(fornecedor1);

        Casa casa1 = new Casa("Artur", "250", "EDP");
        Casa casa2 = new Casa("Afonso", "251", "EDP");
        estado1.adicionaCasa(casa1);
        estado1.adicionaCasa(casa2);
        estado2.adicionaCasa(casa1);
        estado2.adicionaCasa(casa2);

        assertFalse(estado1.equals(estado2));
    }
    @Test
    void maiorConsumidorPeriodo() throws FornecedorInexistenteException, ExisteCasaException, ExisteFornecedorException {
        Fornecedor f1 = new Fornecedor("EDP");
        Casa c = new Casa("Artur", "250", "EDP");
        Casa c2 = new Casa("Afonso", "251", "EDP");
        Casa c3 = new Casa("Goncalo", "252", "EDP");
        Fatura f = new Fatura("EDP", "250", 120, 100, LocalDate.of(2020, 1, 1), LocalDate.of(2020, 2, 1));
        Fatura f2 = new Fatura("EDP", "250", 180, 100, LocalDate.of(2020, 2, 1), LocalDate.of(2020, 3, 1));
        Fatura f3 = new Fatura("EDP", "250", 190, 100, LocalDate.of(2020, 3, 1), LocalDate.of(2020, 4, 1));
        Fatura f4 = new Fatura("EDP", "250", 80, 100, LocalDate.of(2020, 1, 1), LocalDate.of(2020, 2, 1));
        Fatura f5 = new Fatura("EDP", "250", 150, 100, LocalDate.of(2018, 1, 1), LocalDate.of(2020, 1, 1));
        c.adicionaFatura(f);
        c.adicionaFatura(f2);
        c2.adicionaFatura(f3);
        c2.adicionaFatura(f5);
        c3.adicionaFatura(f4);
        EstadoPrograma e = new EstadoPrograma();
        e.addFornecedor(f1);
        e.adicionaCasa(c);
        e.adicionaCasa(c2);
        e.adicionaCasa(c3);
        assertEquals(Arrays.asList(c, c2), e.maiorConsumidorPeriodo(LocalDate.of(2020, 1, 1), LocalDate.of(2021, 1, 1), 2));
    }

    @Test
    void getFornecedorMaiorFaturacao() throws FornecedorInexistenteException, ExisteCasaException, ExisteFornecedorException {
        Fornecedor fornecedor2 = new Fornecedor("EDP");
        Fatura f = new Fatura("EDP", "250", 120, 100, LocalDate.of(2020, 1, 1), LocalDate.of(2020, 2, 1));
        Fatura f2 = new Fatura("EDP", "250", 180, 100, LocalDate.of(2020, 2, 1), LocalDate.of(2020, 3, 1));
        Fatura f3 = new Fatura("Endesa", "250", 190, 100, LocalDate.of(2020, 3, 1), LocalDate.of(2020, 4, 1));
        Fatura f4 = new Fatura("Endesa", "250", 80, 100, LocalDate.of(2020, 1, 1), LocalDate.of(2020, 2, 1));
        Fatura f5 = new Fatura("Endesa", "250", 150, 100, LocalDate.of(2018, 1, 1), LocalDate.of(2020, 1, 1));
        Fornecedor fornecedor1 = new Fornecedor("Endesa");
        fornecedor2.adicionaFatura(f);
        fornecedor2.adicionaFatura(f2);
        fornecedor1.adicionaFatura(f3);
        fornecedor1.adicionaFatura(f4);
        fornecedor1.adicionaFatura(f5);
        EstadoPrograma e = new EstadoPrograma();
        e.addFornecedor(fornecedor1);
        e.addFornecedor(fornecedor2);
        assertEquals(fornecedor1, e.getFornecedorMaiorFaturacao());

    }

    @Test
    void podiumDeviceMaisUsado() throws AlreadyExistDeviceException, FornecedorInexistenteException, ExisteCasaException, ExisteFornecedorException {

        SmartDevice sb1 = new SmartBulb();
        SmartDevice sb2 = new SmartBulb();
        SmartDevice sb3 = new SmartBulb();
        SmartDevice sc1 = new SmartCamera();
        SmartDevice sc2 = new SmartCamera();
        SmartDevice ss = new SmartSpeaker();
        Casa c1 = new Casa("Artur", "256", "EDP");
        Casa c2 = new Casa("Afonso", "257", "EDP");
        Casa c3 = new Casa("Goncalo", "258", "EDP");
        c1.addDevice(sb1);
        c1.addDevice(sc2);
        c2.addDevice(sc1);
        c3.addDevice(sb2);
        c2.addDevice(sb3);
        c3.addDevice(ss);
        EstadoPrograma e = new EstadoPrograma();
        e.addFornecedor(new Fornecedor("EDP"));
        e.adicionaCasa(c1);
        e.adicionaCasa(c2);
        e.adicionaCasa(c3);
        assertEquals(Arrays.asList("SmartBulb", "SmartCamera", "SmartSpeaker"), e.podiumDeviceMaisUsado());

    }
    }
