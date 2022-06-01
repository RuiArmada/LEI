package TP_04.src;

public class Main {

    public static void main(String[] args) {
        Barrier b = new Barrier(3);

        new Thread(() -> {
            try {
                Thread.sleep(1000);
                System.out.println("I shall do a Await");
                b.awaits();
            } catch (Exception e) {}
            System.out.println("Await returned");
            try {
                Thread.sleep(2000);
                System.out.println("I shall do a Await");
                b.awaits();
            } catch (Exception e) {}
            System.out.println("Await returned");
        }).start();

        new Thread(() -> {
            try {
                Thread.sleep(3000);
                System.out.println("I shall do a Await");
                b.awaits();
            } catch (Exception e) {}
            System.out.println("Await returned");
            try {
                Thread.sleep(1000);
                System.out.println("I shall do a Await");
                b.awaits();
            } catch (Exception e) {}
            System.out.println("Await returned");
        }).start();

        new Thread(() -> {
            try {
                Thread.sleep(2000);
                System.out.println("I shall do a Await");
                b.awaits();
            } catch (Exception e) {}
            System.out.println("Await returned");
            try {
                Thread.sleep(3000);
                System.out.println("I shall do a Await");
                b.awaits();
            } catch (Exception e) {}
            System.out.println("Await returned");
        }).start();
    }

}
