package paralel;

public class Ficha extends Thread {

	static int numeroFicha = 0;
	static Ficha a[] = new Ficha[100];
	private int minhaFicha = 0;
	public Ficha() {
		this.minhaFicha = minhaFicha;
	}

	private synchronized int obterFicha() {
		this.minhaFicha = numeroFicha++;
		return this.minhaFicha;

	}

	public void run() {
		if(numeroFicha - this.minhaFicha > 100)
			this.setPriority(MAX_PRIORITY);
		else
			this.setPriority(MIN_PRIORITY);
		for (int i = 0; i < 1000; i++) {
			obterFicha();
				//System.out.println("Thread: "+getName()+"   -"+ );
		}

	}

	public static void main(String[] args) {

		for (int i = 0; i < 100; i++) {
			a[i] = new Ficha(); 
			a[i].setName("T"+i);
			a[i].start();
		}
		
		for (int i = 0; i < a.length; i++) {
			System.out.println("Nome: " + a[i].getName() + " - "+  a[i].minhaFicha);
		}
		
		

	}
	
	
	/* Safety - Propriedade que trata da situação onde duas threads tentam acessar um espaço de memória compartilhado
	 * como por exemplo um contador sequêncial e elas terminam obtendo o mesmo valor onde elas deveriam obter valores
	 * em sequência, o exemplo acima garante que isto não irá acontecer com a utilização do synchronized no método obterFicha*/
	
	/*Liveness - Propriedade que trata a situação onde a execução paralela de programas sempre resultará em algo bom
	 * Uma forma de uma falha de vivacidade é quando uma thread bloqueia um recurso e não libera para outras threads o acessarem
	 *, meu códido previne essa situação setando a prioridade com qual as threads possuem executarão pela sua ficha atual, se
	 *uma thread possui uma ficha próxima a última ficha  entregue sua prioridade é setada ao mínimo de forma que threads
	 *com fichas mais antigas peguem fichas com mais facilidade*/

}
