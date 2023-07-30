package smart_houses;

import smart_houses.exceptions.AlreadyExistDeviceException;
import smart_houses.exceptions.DeviceInexistenteException;
import smart_houses.exceptions.ExisteCasaException;
import smart_houses.exceptions.ExisteFornecedorException;
import smart_houses.exceptions.FornecedorInexistenteException;
import smart_houses.exceptions.RoomAlreadyExistsException;
import smart_houses.exceptions.RoomInexistenteException;
import smart_houses.modulo_casas.Casa;
import smart_houses.modulo_fornecedores.Fornecedor;
import smart_houses.smart_devices.SmartBulb;
import smart_houses.smart_devices.SmartCamera;
import smart_houses.smart_devices.SmartSpeaker;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class Parser {

    public EstadoPrograma parse(){
        List<String> linhas = lerFicheiro("./src/main/resources/log.txt");
        String[] linhaPartida;
        String divisao = null;
        Casa casaMaisRecente = null;
        EstadoPrograma estado = new EstadoPrograma();
        for (String linha : linhas) {
            linhaPartida = linha.split(":", 2);
            switch (linhaPartida[0]) {
                case "Fornecedor" -> {
                    Fornecedor f = new Fornecedor(linhaPartida[1]);
                    try {
                        estado.addFornecedor(f);
                    } catch (ExisteFornecedorException e) {
                        e.printStackTrace();
                    }
                }
                case "Casa" -> {
                    if (casaMaisRecente != null) {
                        try {
                            estado.adicionaCasa(casaMaisRecente);
                        } catch (ExisteCasaException | FornecedorInexistenteException e) {
                            e.printStackTrace();
                        }
                    }
                    casaMaisRecente = parseCasa(linhaPartida[1]);
                }
                case "Divisao" -> {
                    if (casaMaisRecente == null) System.out.println("Linha inválida.");
                    divisao = linhaPartida[1];
                    try {
                        casaMaisRecente.addRoom(divisao);
                    } catch (RoomAlreadyExistsException e) {
                        e.printStackTrace();
                    }
                }
                case "SmartBulb" -> {
                    if (divisao == null) System.out.println("Linha inválida.");
                    SmartBulb sd = parseSmartBulb(linhaPartida[1]);
                    try {
                        casaMaisRecente.addDevice(sd);
                    } catch (AlreadyExistDeviceException e) {
                        e.printStackTrace();
                    }
                    try {
                        casaMaisRecente.addDeviceOnRoom(divisao, sd.getId());
                    } catch (RoomInexistenteException e) {
                        e.printStackTrace();
                    } catch (DeviceInexistenteException e) {
                        e.printStackTrace();
                    }
                }
                case "SmartCamera" -> {
                    if (divisao == null) System.out.println("Linha inválida.");
                    SmartCamera sc = parseSmartCamera(linhaPartida[1]);
                    try {
                        casaMaisRecente.addDevice(sc);
                    } catch (AlreadyExistDeviceException e) {
                        e.printStackTrace();
                    }
                    try {
                        casaMaisRecente.addDeviceOnRoom(divisao, sc.getId());
                    } catch (RoomInexistenteException e) {
                        e.printStackTrace();
                    } catch (DeviceInexistenteException e) {
                        e.printStackTrace();
                    }
                }
                case "SmartSpeaker" -> {
                    if (divisao == null) System.out.println("Linha Invalida.");
                    SmartSpeaker ss = parseSmartSpeaker(linhaPartida[1]);
                    try {
                        casaMaisRecente.addDevice(ss);
                    } catch (AlreadyExistDeviceException e) {
                        e.printStackTrace();
                    }
                    try {
                        casaMaisRecente.addDeviceOnRoom(divisao, ss.getId());
                    } catch (RoomInexistenteException e) {
                        e.printStackTrace();
                    } catch (DeviceInexistenteException e) {
                        e.printStackTrace();
                    }
                }
                default -> System.out.println("Linha inválida.");
            }
        }
        if(casaMaisRecente != null) {
            try {
                estado.adicionaCasa(casaMaisRecente);
            } catch (ExisteCasaException e) {
                e.printStackTrace();
            } catch (FornecedorInexistenteException e) {
                e.printStackTrace();
            }
        }
        return estado;
    }

    private SmartSpeaker parseSmartSpeaker(String input) {
        Random r = new Random();
        String[] campos = input.split(",");
        int volume = Integer.parseInt(campos[0]);
        String estacao = campos[1];
        String marca = campos[2];
        double consumo = Double.parseDouble(campos[3]);
        return new SmartSpeaker(r.nextBoolean(), consumo, volume, estacao, marca);
    }

    private SmartCamera parseSmartCamera(String s) {
        Random r = new Random();
        String[] campos = s.split(",");
        String res = campos[0];
        String[] camposRes = res.split("x");
        int resX = Integer.parseInt(camposRes[0].substring(1));
        int resY = Integer.parseInt(camposRes[1].substring(0, camposRes[1].length() - 1));
        int tam = Integer.parseInt(campos[1]);
        double consumo = Double.parseDouble(campos[2]);
        return new SmartCamera(r.nextBoolean(), consumo, resX, resY, tam);
    }

    public SmartBulb parseSmartBulb(String linha){
        Random r = new Random();
        String[] campos = linha.split(",");
        String tone = campos[0];
        SmartBulb.Tones t = SmartBulb.Tones.WARM;
        if(tone.equals("Warm")) t = SmartBulb.Tones.WARM;
        else if(tone.equals("Neutral")) t = SmartBulb.Tones.NEUTRAL;
        else t = SmartBulb.Tones.COLD;
        int dimensao = Integer.parseInt(campos[1]);
        double consumo = Double.parseDouble(campos[2]);
        return new SmartBulb(r.nextBoolean(), consumo, t, dimensao);
    }

    public List<String> lerFicheiro(String nomeFich) {
        List<String> lines;
        try { lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8); }
        catch(IOException exc) { lines = new ArrayList<>(); }
        return lines;
    }

    public Casa parseCasa(String input){
        String[] campos = input.split(",");
        String nome = campos[0];
        String nif = campos[1];
        String fornecedor = campos[2];
        return new Casa(nome, nif, fornecedor);
    }
}
