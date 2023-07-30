import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.AfterEach; 
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * The test class SmartCameraTest
 *
 * @author  grupoPOO 47
 * @version 20220514
 */
public class SmartCameraTest 
{
    SmartCamera devCamera;

    /**
     * Construtor default para a classe de teste SmartCameraTest
     */
    public SmartCameraTest(){}

    /**
     * Cria uma SmartCamera.
     * Chamado antes de cada método em caso de teste
     */
    @BeforeEach
    public void setUp()
    {
        devCamera = new SmartCamera(500, 1080, 920, 60, 4.5);
    }

    /**
     * Derruba o dispositivo de teste
     * Chamado após cada método de teste
     */
    @AfterEach
    public void tearDown(){}

    /**
     * Método que testa o construtor vazio da classe SmartCamera
     */
    @Test
    public void testSmartCameraVazio()
    {
        SmartCamera devCamera1 = new SmartCamera();
        assertEquals(devCamera1.getX(), 0, "X");
        assertEquals(devCamera1.getY(), 0, "Y");
        assertEquals(devCamera1.getTamanho(), 0.0, "Tamanho");
        assertEquals(devCamera1.getConsumption(), 0.0, "Consumption");
    }

    /**
     * Método que testa o constructor parametrizado da classe SmartCamera
     */
    @Test
    public void testSmartCameraParam()
    {
        assertEquals(devCamera.getX(), 1080, "X");
        assertEquals(devCamera.getY(), 920, "Y");
        assertEquals(devCamera.getTamanho(), 60, "Tamanho");
        assertEquals(devCamera.getConsumption(), 4.5, "Consumption");
    }

    /**
     * Método que testa o construtor cópia da classe SmartCamera
     */
    @Test
    public void testSmartCameraCopia()
    {
        SmartCamera devCamera2 = new SmartCamera(devCamera);
        assertEquals(devCamera2.getX(), 1080, "X");
        assertEquals(devCamera2.getY(), 920, "Y");
        assertEquals(devCamera2.getTamanho(), 60, "Tamanho");
        assertEquals(devCamera2.getConsumption(), 4.5, "Consumption");

        assertEquals(devCamera, devCamera2, "Os objetos são iguais");
    }

    /**
     * Método que testa a devolução do consumo da SmartCamera
     */
    @Test
    void testGetConsumption() 
    {
        assertEquals(devCamera.getConsumption(), 4.5);
        devCamera = new SmartCamera(500, 1080, 920, 60, -4.5);
        assertEquals(devCamera.getConsumption(), 0);
        devCamera = new SmartCamera();
        assertEquals(devCamera.getConsumption(), 0);

    }

    /**
     * Método que testa a devolução do tamanho do ficheiro da SmartCamera
     */
    @Test
    void testGetTamanho() 
    {
        assertEquals(devCamera.getTamanho(), 60);
        devCamera = new SmartCamera(500, 1080, 920, -4, 4.5);
        assertEquals(devCamera.getTamanho(), 0);
        devCamera = new SmartCamera();
        assertEquals(devCamera.getTamanho(), 0);
    }

    /**
     * Método que testa a devolução da primeira variável da resolução da SmartCamera
     */
    @Test
    void testGetX() 
    {
        assertEquals(devCamera.getX(), 1080);
        devCamera = new SmartCamera(500, -8, 920, 60, 4.5);
        assertEquals(devCamera.getX(), 0);
        devCamera = new SmartCamera();
        assertEquals(devCamera.getX(), 0);
    }

    /**
     * Método que testa a devolução da segunda variável da resolução da SmartCamera
     */
    @Test
    void testGetY() 
    {
        assertEquals(devCamera.getY(), 920);
        devCamera = new SmartCamera(500, 1080, -9, 60, 4.5);
        assertEquals(devCamera.getY(), 0);
        devCamera = new SmartCamera();
        assertEquals(devCamera.getY(), 0);
    }

    /**
     * Método que testa a atualização do consumo da SmartCamera
     */
    @Test
    void testSetConsumption() 
    {
        devCamera.setConsumption(5.1);
        assertEquals(5.1, devCamera.getConsumption());
        devCamera.setConsumption(0);
        assertEquals(0, devCamera.getConsumption());

    }

    /**
     * Método que testa a atualização do tamanho do ficheiro da SmartCamera
     */
    @Test
    void testSetTamanho()
    {
        devCamera.setTamanho(55);
        assertEquals(55, devCamera.getTamanho());
        devCamera.setTamanho(0);
        assertEquals(0, devCamera.getTamanho());
    }

    /**
     * Método que testa a atualiza da primeira variável da resolução da SmartCamera
     */
    @Test
    void testSetX() 
    {
        devCamera.setX(720);
        assertEquals(720, devCamera.getX());
        devCamera.setX(0);
        assertEquals(0, devCamera.getX());
    }

    /**
     * Método que testa a atualização da segunda variável da resolução da SmartCamera
     */
    @Test
    void testSetY() 
    {
        devCamera.setY(480);
        assertEquals(480, devCamera.getY());
        devCamera.setY(0);
        assertEquals(0, devCamera.getY());
    }
}
