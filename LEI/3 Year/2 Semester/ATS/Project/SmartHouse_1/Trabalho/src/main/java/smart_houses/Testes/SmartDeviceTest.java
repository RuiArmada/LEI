package smart_houses.Testes;

import org.junit.jupiter.api.Test;
import smart_houses.smart_devices.SmartBulb;
import smart_houses.smart_devices.SmartCamera;
import smart_houses.smart_devices.SmartDevice;
import smart_houses.smart_devices.SmartSpeaker;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class SmartDeviceTest {

    @Test
    void comsumption() {
        SmartDevice sb1 = new SmartBulb(true, 0.20, SmartBulb.Tones.NEUTRAL, 20);
        SmartDevice sb2 = new SmartBulb(true, 0.20, SmartBulb.Tones.WARM, 20);
        SmartDevice sb3 = new SmartBulb(true, 0.20, SmartBulb.Tones.COLD, 20);

        assertEquals(0.23, sb1.comsumption());
        assertEquals(0.25, sb2.comsumption());
        assertEquals(0.22, sb3.comsumption());

        SmartDevice sc = new SmartCamera(true, 0.20, 1920, 1080, 50);
        assertEquals(2.3236, sc.comsumption());

        SmartDevice ss = new SmartSpeaker(true, 0.20, 70, "RFM", "Sony");

        assertEquals(0.27, ss.comsumption());
    }
    @Test
    void SmartDeviceTest(){
        SmartDevice d = new SmartBulb();
        assertFalse(d.isOn());
        assertEquals(0.0, d.getConsume());
    }
    @Test
    void SmartDeviceTest1(){
    SmartSpeaker d =new SmartSpeaker(true, 0.20, 70, "RFM", "Sony");
    assertTrue (d.isOn());
    assertEquals(0.20, d.getConsume(), 0.5);
    assertEquals(70, d.getVolume());
    assertEquals("RFM", d.getRadioStation());
    assertEquals("Sony", d.getBrand());
    }
    @Test
    void SmartDeviceTest2(){
        SmartCamera d= new SmartCamera(true, 0.20,60, 60,500);
        assertTrue (d.isOn());
        assertEquals(0.20, d.getConsume(), 0.5);
        assertEquals(60, d.getResolutionX());
        assertEquals(60, d.getResolutionY());
        assertEquals(500, d.getFileDim());
    }
    @Test
    void SmartDeviceTest3(){
        SmartBulb d = new SmartBulb(true, 0.20, SmartBulb.Tones.NEUTRAL, 20);
        assertTrue (d.isOn());
        assertEquals(0.20, d.getConsume(), 0.5);
        assertEquals(SmartBulb.Tones.NEUTRAL, d.getTone());
        assertEquals(20, d.getDimension());
    }
    @Test
    void setONTest(){
        SmartDevice d= new SmartBulb();
        assertFalse(d.isOn());
        d.setOn(true);
        assertTrue(d.isOn());
    }
    @Test
    void getConsumeTest(){
        SmartBulb d = new SmartBulb(true, 0.20, SmartBulb.Tones.NEUTRAL, 20);
        assertEquals(0.20, d.getConsume(), 0.50);
    }
    @Test
    void setConsumeTest(){
        SmartBulb d = new SmartBulb(true, 0.20, SmartBulb.Tones.NEUTRAL, 20);
        assertEquals(0.20, d.getConsume(), 0.50);
        d.setConsume(0.40);
        assertEquals(0.40, d.getConsume(), 0.50);
    }
    @Test
    void toStringTest1(){
        SmartBulb d = new SmartBulb(true, 0.20, SmartBulb.Tones.NEUTRAL, 20);
        String expected= "SmartBulb{" + "id=" + d.getId() + ", on=true, tone=NEUTRAL, dimension=20, consume=0.2}";
        assertEquals(expected, d.toString());
    }
    @Test
    void toStringTest2(){
        SmartCamera d = new SmartCamera(true, 0.20, 20,20,500);
        String expected= "SmartCamera{" + "id=" + d.getId() + ", on=true, resolutionX=20, resolutionY=20, fileDim=500}";
        assertEquals(expected, d.toString());
    }
    @Test
    void toStringTest3(){
        SmartSpeaker d = new SmartSpeaker (true, 0.20,50, "RFM", "Sony");
        String expected= "SmartSpeaker{" + "id = " + d.getId() + ", on = truevolume=50, radioStation='RFM', brand='Sony'}";
        assertEquals(expected, d.toString());
    }
    @Test
    void equalsTest1(){
        SmartSpeaker d1 = new SmartSpeaker (true, 0.20,50, "RFM", "Sony");
        SmartSpeaker d2 = new SmartSpeaker (true, 0.40,100, "RFM", "Sony");
        SmartSpeaker d3 = new SmartSpeaker (d1);
        assertFalse(d1.equals(d2));
        assertTrue(d1.equals(d3));
    }
    @Test
    void equalsTest2(){
        SmartCamera d1 = new SmartCamera(true, 0.20, 20,20,500);
        SmartCamera d2 = new SmartCamera(true, 0.40, 50,30,500);
        SmartCamera d3 = new SmartCamera (d1);
        assertFalse(d1.equals(d2));
        assertTrue(d1.equals(d3));
    }
    @Test
    void equalsTest3(){
        SmartSpeaker d1 =new SmartSpeaker(true, 0.20, 70, "RFM", "Sony");
        SmartSpeaker d2 =new SmartSpeaker(true, 0.40, 80, "RFM", "Sony");
        SmartSpeaker d3 = new SmartSpeaker (d1);
        assertFalse(d1.equals(d2));
        assertTrue(d1.equals(d3));
    }
    @Test
    void cloneTest(){
        SmartSpeaker d1 =new SmartSpeaker(true, 0.20, 70, "RFM", "Sony");
        SmartCamera d2 = new SmartCamera(true, 0.20, 20,20,500);
        SmartSpeaker d3 = new SmartSpeaker (true, 0.20,50, "RFM", "Sony");
        SmartSpeaker d4= d1.clone();
        SmartCamera d5= d2.clone();
        SmartSpeaker d6= d3.clone();
        assertTrue(d4.equals(d1));
        assertTrue(d5.equals(d2));
        assertTrue(d6.equals(d3));
        assertTrue(d4!=d1);
        assertTrue(d5!=d2);
        assertTrue(d6!=d3);


    }

}