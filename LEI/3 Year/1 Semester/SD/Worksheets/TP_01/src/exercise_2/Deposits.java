package TP_01.src.exercise_2;

public class Deposits implements Runnable {
    
    private Bank bank;
    int deposit;
    int n_deposits;

    public Deposits(Bank bk, int dep, int n) {
        this.bank = bk;
        this.deposit = dep;
        this.n_deposits = n;
    }

    public void run() {
        for(int i = 0 ; i < n_deposits ; i++)
            bank.deposit(this.deposit);
    }

}
