package smart_houses.Testes;

import org.junit.jupiter.api.Test;
import smart_houses.Fatura;
import smart_houses.exceptions.*;
import smart_houses.modulo_casas.Casa;
import smart_houses.smart_devices.SmartBulb;
import smart_houses.smart_devices.SmartCamera;
import smart_houses.smart_devices.SmartDevice;
import smart_houses.smart_devices.SmartSpeaker;

import java.time.LocalDate;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

class CasaTest {

    @Test
    void adicionaFatura() {
        Casa c = new Casa("Artur", "256250278", "EDP");
        Fatura f1 = new Fatura();
        Fatura f2 = new Fatura();
        Fatura f3 = new Fatura();

        c.adicionaFatura(f1);
        c.adicionaFatura(f2);
        c.adicionaFatura(f3);

        assertEquals(Arrays.asList(f1, f2, f3), c.getFaturas());
    }

    @Test
    void testEquals() {
        Casa c = new Casa("Artur", "23", "EDP");
        Casa c2 = new Casa("Artur", "23", "EDP");

        assertEquals(c, c2);
    }

    @Test
    void setAllDevicesStateRoom() throws RoomAlreadyExistsException, AlreadyExistDeviceException, DeviceInexistenteException, RoomInexistenteException {

        Casa c = new Casa("Artur", "23", "EDP");

        SmartDevice d = new SmartCamera();
        SmartDevice d2 = new SmartCamera();
        SmartDevice d3 = new SmartCamera();
        SmartDevice d4 = new SmartCamera();
        c.addRoom("Quarto");
        c.addDevice(d);
        c.addDevice(d2);
        c.addDevice(d3);
        c.addDevice(d4);
        c.addDeviceOnRoom("Quarto", d.getId());
        c.addDeviceOnRoom("Quarto", d2.getId());
        c.addDeviceOnRoom("Quarto", d3.getId());
        c.addDeviceOnRoom("Quarto", d4.getId());
        c.setAllDevicesStateRoom("Quarto", true);
        assertTrue(c.getListDevices().stream().allMatch(SmartDevice::isOn));
    }

    @Test
    void setAllDevicesState() throws AlreadyExistDeviceException {
        Casa c = new Casa("Artur", "23", "EDP");

        SmartDevice d = new SmartCamera();
        SmartDevice d2 = new SmartCamera();
        SmartDevice d3 = new SmartCamera();
        SmartDevice d4 = new SmartCamera();
        c.addDevice(d);
        c.addDevice(d2);
        c.addDevice(d3);
        c.addDevice(d4);
        assertEquals(Arrays.asList(d, d2, d3, d4), c.getListDevices());
        c.setAllDevicesState(true);
        assertTrue(c.getListDevices().stream().allMatch(SmartDevice::isOn));
    }

    @Test
    void setDeviceState() throws AlreadyExistDeviceException, DeviceInexistenteException {
        Casa c = new Casa("Artur", "23", "EDP");

        SmartDevice d = new SmartCamera();
        SmartDevice d2 = new SmartCamera();
        SmartDevice d3 = new SmartCamera();
        SmartDevice d4 = new SmartCamera();
        c.addDevice(d);
        c.addDevice(d2);
        c.addDevice(d3);
        c.addDevice(d4);
        assertEquals(Arrays.asList(d, d2, d3, d4), c.getListDevices());
        c.setDeviceState(d.getId(), true);
        assertTrue(c.getDevice(d.getId()).isOn());
    }

    @Test
    void consumoDispositivos() throws AlreadyExistDeviceException {
        Casa c = new Casa("Artur", "23", "EDP");
        SmartDevice d = new SmartBulb(true, 0.20, SmartBulb.Tones.NEUTRAL, 20);
        SmartDevice d2 = new SmartBulb(true, 0.20, SmartBulb.Tones.NEUTRAL, 20);
        c.addDevice(d);
        c.addDevice(d2);
        assertEquals(d.comsumption() + d2.comsumption(), c.consumoDispositivos());
    }

    @Test
    void consumoPeriodo() {
        Casa c = new Casa("Artur", "23", "EDP");
        Fatura f = new Fatura("EDP", "23", 20, 10, LocalDate.of(2010, 1, 1), LocalDate.of(2012, 1, 1));
        Fatura f2 = new Fatura("EDP", "23", 20, 40, LocalDate.of(2012, 1, 1), LocalDate.of(2014, 1, 1));
        c.adicionaFatura(f);
        c.adicionaFatura(f2);
        assertEquals(20.0, c.consumoPeriodo());
    }

    @Test
    void testConsumoPeriodo() {
        Casa c = new Casa("Artur", "23", "EDP");
        Fatura f = new Fatura("EDP", "23", 20, 10, LocalDate.of(2010, 1, 1), LocalDate.of(2012, 1, 1));
        Fatura f2 = new Fatura("EDP", "23", 20, 40, LocalDate.of(2012, 1, 1), LocalDate.of(2014, 1, 1));
        Fatura f3 = new Fatura("EDP", "23", 20, 40, LocalDate.of(2008, 1, 1), LocalDate.of(2010, 1, 1));
        c.adicionaFatura(f);
        c.adicionaFatura(f2);
        c.adicionaFatura(f3);
        assertEquals(40.0, c.consumoPeriodo(LocalDate.of(2010, 1, 1),LocalDate.of(2020, 1, 1)));
    }

    @Test
    void mudaDeviceDeRoom() throws RoomAlreadyExistsException, AlreadyExistDeviceException, DeviceInexistenteException, RoomInexistenteException {
        Casa c = new Casa();
        SmartDevice sm = new SmartBulb();
        c.addRoom("Quarto");
        c.addRoom("Sala");
        c.addDevice(sm);
        c.addDeviceOnRoom("Quarto", sm.getId());
        assertTrue(c.getRooms().get("Quarto").contains(sm.getId()));

        c.mudaDeviceDeRoom("Sala", sm.getId());
        assertTrue(c.getRooms().get("Sala").contains(sm.getId()));
    }

    @Test
    void juntaRooms() throws DeviceInexistenteException, RoomInexistenteException, AlreadyExistDeviceException, RoomAlreadyExistsException {
        Casa c = new Casa();
        SmartDevice sm = new SmartBulb();
        SmartDevice sm2 = new SmartBulb();
        c.addRoom("Quarto");
        c.addRoom("Sala");
        c.addDevice(sm);
        c.addDeviceOnRoom("Quarto", sm.getId());
        c.addDevice(sm2);
        c.addDeviceOnRoom("Sala", sm2.getId());
        c.mudaDeviceDeRoom("Sala", sm.getId());

        c.juntaRooms("Quarto", "Sala", "Cozinha");
        assertFalse(c.getRooms().containsKey("Quarto"));
        assertFalse(c.getRooms().containsKey("Sala"));
        assertTrue(c.getRooms().containsKey("Cozinha"));
        assertTrue(c.getRooms().get("Cozinha").contains(sm.getId()));
        assertTrue(c.getRooms().get("Cozinha").contains(sm2.getId()));

    }

    //Adicionados pós Pit
    @Test
    void checkRooms() throws RoomAlreadyExistsException {
        Casa c = new Casa();
        c.addRoom("Quarto");
        c.addRoom("Sala");
        assertSame(2, c.getRooms().size());
    }

    @Test
    void checkDevices() throws DeviceInexistenteException, RoomInexistenteException, AlreadyExistDeviceException, RoomAlreadyExistsException {
        Casa c = new Casa();
        SmartDevice sm = new SmartBulb();
        SmartDevice sm2 = new SmartBulb();
        c.addRoom("Quarto");
        c.addRoom("Sala");
        c.addDevice(sm2);
        c.addDevice(sm);
        assertSame(2, c.getMapDevices().size());
    }

    @Test
    void checkEquals() {
        Casa c = new Casa("Artur", "23", "EDP");
        assertTrue(c.equals(new Casa("Artur", "23", "EDP")));
    }

    @Test
    void setFornecedorTest() {
        Casa c = new Casa("Artur", "23", "EDP");
        String newFornecedor = "EDP";
        c.setFornecedor(newFornecedor);
        assert c.getFornecedor().equals(newFornecedor);
    }

    @Test
    void addDeviceTest() throws AlreadyExistDeviceException {
        Casa c = new Casa("Artur", "123", "EDP");
        SmartDevice d2 = new SmartBulb(true, 0.20, SmartBulb.Tones.NEUTRAL, 20);
        c.addDevice(d2);
        assert c.getMapDevices().containsKey(d2.getId());

    }

    @Test
    void removeDeviceTest() throws AlreadyExistDeviceException, DeviceInexistenteException {
        Casa c = new Casa("Artur", "123", "EDP");
        SmartDevice d2 = new SmartBulb(true, 0.20, SmartBulb.Tones.NEUTRAL, 20);
        c.addDevice(d2);
        c.removeDevice(d2.getId());
        assert !c.getMapDevices().containsKey(d2.getId());
    }
    @Test
    void addRoomTest() throws RoomAlreadyExistsException {
        Casa c = new Casa("Artur", "123", "EDP");
        String room = "Sala";
        c.addRoom(room);
        assert c.getRooms().containsKey(room);
    }
    @Test
    void removeRoomTest() throws RoomAlreadyExistsException {
        Casa c = new Casa("Artur", "123", "EDP");
        String room = "Sala";
        c.addRoom(room);
        c.removeRoom(room);
        assert !c.getRooms().containsKey(room);

    }
    @Test
    void getFaturasTest(){
        Casa c = new Casa("Artur", "123", "EDP");
        List<Fatura> faturas = c.getFaturas();
        assert faturas != null;
    }
    @Test
    void getFornecedorTest(){
        Casa c = new Casa("Artur", "123", "EDP");
        String fornecedor = c.getFornecedor();
        assert fornecedor.equals("EDP");
    }
    @Test
    void getNomeTest(){
        Casa c = new Casa("Artur", "123", "EDP");
        String nome = c.getNome();
        assert nome.equals("Artur");
    }
    @Test
    void equalsTest(){
        Casa c = new Casa("Artur", "123", "EDP");
        Casa c1 = new Casa("Artur", "123", "EDP");
        assert c.equals(c1);
    }
    @Test
    void toStringTest(){
        Casa c = new Casa("Artur", "123", "EDP");
        String casaString = c.toString();
        assert casaString != null;
    }
    @Test
    void getMapDevicesTest(){
        Casa c = new Casa("Artur", "123", "EDP");
        Map<Integer, SmartDevice> devices = c.getMapDevices();
        assert devices != null;
    }
    @Test
    void getListDevicesTest(){
        Casa c = new Casa("Artur", "123", "EDP");
        List<SmartDevice> devices = c.getListDevices();
        assert devices != null;
    }
    @Test
    void getListRoomsTest(){
        Casa c = new Casa("Artur", "123", "EDP");
        List<String> rooms = c.getListRooms();
        assert rooms != null;
    }
    @Test
    void setDevicesTest(){
        Casa c = new Casa("Artur", "123", "EDP");
        Map<Integer, SmartDevice> devices = new HashMap<>();
        devices.put(1, new SmartBulb(true, 0.20, SmartBulb.Tones.NEUTRAL, 20));
        devices.put(2, new SmartSpeaker(true, 0.20, 70, "RFM", "Sony"));
        c.setDevices(devices);
        assert c.getMapDevices().equals(devices);
    }
    @Test
    void getRoomsTest(){
        Casa c = new Casa("Artur", "123", "EDP");
        Map<String, Set<Integer>> rooms = c.getRooms();
        assert rooms != null;
    }
    @Test
    void addDeviceOnRoomTest() throws RoomAlreadyExistsException, DeviceInexistenteException, RoomInexistenteException, AlreadyExistDeviceException {
        Casa c = new Casa("Artur", "123", "EDP");
        String room ="Sala";
        c.addRoom(room);
        SmartDevice d2 = new SmartBulb();
        c.addDevice(d2);
        c.addDeviceOnRoom(room, d2.getId());
        Set<Integer> tmp = new HashSet<>();
        tmp= c.getRooms().get("Sala");
        System.out.println(tmp.toString());
        assertSame(1, c.getRooms().values().size());
        assertTrue(tmp.contains(d2.getId()));
    }
    @Test
    void removeDeviceOnRoomTest() throws RoomAlreadyExistsException, AlreadyExistDeviceException, DeviceInexistenteException, RoomInexistenteException {
        Casa c = new Casa("Artur", "123", "EDP");
        String room ="Sala";
        c.addRoom(room);
        SmartDevice d2 = new SmartBulb();
        c.addDevice(d2);
        c.addDeviceOnRoom(room, d2.getId());
        Set<Integer> tmp = new HashSet<>();
        tmp= c.getRooms().get("Sala");
        System.out.println(tmp.toString());
        assertTrue(tmp.contains(d2.getId()));
        c.removeDeviceOnRoom(d2.getId());
        assertTrue(c.getRooms().get("Sala").isEmpty());
    }
    @Test
    void divisaoDeDispositivoTest() throws RoomAlreadyExistsException, AlreadyExistDeviceException, DeviceInexistenteException, RoomInexistenteException {
        Casa c = new Casa("Artur", "123", "EDP");
        String room ="Sala";
        c.addRoom(room);
        SmartDevice d2 = new SmartBulb();
        c.addDevice(d2);
        c.addDeviceOnRoom(room, d2.getId());
        assertSame("Sala", c.divisaoDeDispositivo(d2.getId()));
    }
    @Test
    void alteraInfoBulbTest() throws AlreadyExistDeviceException, DeviceInexistenteException, TipoDeviceErradoException {
        Casa c = new Casa("Artur", "123", "EDP");
        SmartDevice d= new SmartBulb(true, 0.20, SmartBulb.Tones.NEUTRAL, 20);
        c.addDevice(d);
        c.alteraInfoBulb(d.getId(), device->device.setConsume(0.40));
        assertEquals(0.40, c.getDevice(d.getId()).getConsume(), 0.5);
    }
    @Test
    void alteraInfoCameraTest() throws AlreadyExistDeviceException, DeviceInexistenteException, TipoDeviceErradoException {
        Casa c = new Casa("Artur", "123", "EDP");
        SmartDevice d= new SmartCamera(true, 0.20, 70, 70, 500);
        c.addDevice(d);
        c.alteraInfoCamera(d.getId(), device-> device.setConsume(0.40));
        assertEquals(0.40, c.getDevice(d.getId()).getConsume(), 0.5);
    }
    @Test
    void alteraInfoSpeakerTest() throws RoomAlreadyExistsException, AlreadyExistDeviceException, DeviceInexistenteException, TipoDeviceErradoException {
        Casa c = new Casa("Artur", "123", "EDP");
        SmartDevice d= new SmartSpeaker(true, 0.20, 70, "RFM", "Sony");
        c.addDevice(d);
        c.alteraInfoSpeaker(d.getId(), device-> device.setConsume(0.40));
        assertEquals(0.40, c.getDevice(d.getId()).getConsume(), 0.5);
    }


}