package exercicios;

import java.util.Random;
import java.util.Vector;
import java.util.concurrent.locks.*;

public class Vetor {

	private Vector<Integer> safeVetor;
	private Vector<Lock> lockList;

	public Vetor(int size) {
		safeVetor = new Vector<Integer>(size,size);
		
		lockList = new Vector<Lock>(size,size);
		for (int i = 0; i < size; i++) {
			safeVetor.addElement(0);
			lockList.addElement(new ReentrantLock());
		}
	}

	public void escrever(int index, int toWrite) {
		lockList.get(index).lock();
		try {
			safeVetor.add(index, toWrite);
			System.out.println(Thread.currentThread().getName()+" adicionou na posição: " + index + " o valor: "
					+ toWrite);
		} finally {
			lockList.get(index).unlock();
		}
	}

	public void ler(int index) {
		lockList.get(index).lock();
		try {
			System.out.println(Thread.currentThread().getName()+" leu o valor: " + safeVetor.get(index)
					+ " da posição:" + index);
		} finally {
			lockList.get(index).unlock();
		}
	}

	public void swap(int i1, int i2) {
		boolean l1 = lockList.get(i1).tryLock();
		boolean l2 = lockList.get(i2).tryLock();

		try {
			while (!(l1 && l2)) {
				
				if (l1)
					lockList.get(i1).unlock();
				if(l2)
					lockList.get(i2).unlock();
				
				l1 = lockList.get(i1).tryLock();
				l2 = lockList.get(i2).tryLock();
			}
			
			int temp = safeVetor.get(i2);
			safeVetor.set(i2, safeVetor.get(i1));
			safeVetor.set(i1, temp);
			System.out.println(Thread.currentThread().getName()+ " Trocou " + temp + " no índice " + i2 + " com " + safeVetor.get(i2) + " no índice " + i1 + ".");

		} finally {
			if (l1)
				lockList.get(i1).unlock();
			if (l2)
				lockList.get(i2).unlock();
		}

	}
	
	
	static class Teste extends Thread{
		private Vetor vetor ;
		
		public Teste(Vetor vetor){this.vetor = vetor;
		}
		
		public void run(){
			for (int i = 0; i < 10; i++) {
				Random random = new Random();
					int m = random.nextInt(3);

					if(m == 0){
						vetor.ler(random.nextInt(50));
					} else if (m == 1){
						vetor.escrever(random.nextInt(50), random.nextInt(100));
					} else if (m == 2) {
						int i1 = random.nextInt(50);
						int i2 = random.nextInt(50);
						while (i1 == i2){
							i1 = random.nextInt(50);
							i2 = random.nextInt(50);
						}
						vetor.swap(i1, i2);
							
				}
				
			}
		}
	}
	
	
	public static void main(String[] args){
		Vetor vetor = new Vetor(50);
		
		
		Thread t[] = new Thread[10];
		for (int i = 0; i < t.length; i++) {
			t[i] = new Teste(vetor);
			t[i].start();
		}
		
		for (int i = 0; i < t.length; i++) {
			try {
				t[i].join();
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
		
	}
	

}
