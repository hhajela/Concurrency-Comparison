import org.omg.PortableServer.ThreadPolicy;

public class Solution {

	public static void main(String[] args) {
		
	int[][]	grid = {
		  {0,2,0,0,1,16,4,0,0,10,0,0,0,0,12,0},
		  {10,0,0,11,0,6,0,0,2,0,0,0,0,14,0,0},
		  {0,12,0,0,0,0,0,8,0,0,3,0,2,0,0,10},
		  {0,0,0,15,0,0,5,0,1,0,0,0,0,0,4,0},
		  {11,0,12,0,0,14,0,6,0,1,0,3,0,10,0,4},
		  {0,13,0,7,10,0,3,0,0,0,0,16,0,0,5,0},
		  {16,0,15,0,12,0,0,0,8,0,0,0,9,0,0,2},
		  {0,0,0,9,0,11,0,0,0,0,0,15,0,3,0,0,},
		  {0,8,4,0,11,0,12,2,0,5,0,0,14,0,9,0,},
		  {7,0,10,0,0,0,0,0,3,0,0,9,0,15,0,11},
		  {0,6,0,0,15,0,0,16,0,0,10,0,12,0,13,0},
		  {0,0,0,12,0,1,0,0,0,16,0,13,0,6,0,7},
		  {0,0,0,0,0,0,0,0,11,0,5,0,0,0,0,0},
		  {0,0,0,10,13,9,0,5,0,12,0,0,15,0,8,0},
		  {0,7,0,0,8,0,14,0,0,0,15,0,0,0,0,0},
		  {14,0,3,0,0,7,0,0,0,0,13,2,0,4,0,16}
		};
	
	System.out.println(grid.length);
	int size = grid.length;
	Board board = new Board(grid,size);
	BoardHelper bHelper = new BoardHelper();
	Cell[][] local = bHelper.getCopy(board.getBoard());
	
	int numOfThread = 4;
	SodukoSolver[] sk = new SodukoSolver[numOfThread];
	//
	Thread[] thread = new Thread[numOfThread];
	//int thread_waiting = 0;
	
	sk[0]= new SodukoSolver(numOfThread);
	sk[0].addToStack(local);
	long startTime = System.nanoTime();
	
	for(int i=0;i<numOfThread;i++)
		{
		if(i!=0)
			sk[i]=new SodukoSolver(numOfThread);
		thread[i] = new Thread(sk[i]);
		thread[i].start();
		}
	for(int i=0;i<numOfThread;i++)
	{
		try {
			thread[i].join();
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	
	long stopTime = System.nanoTime();
	
	System.out.println("Soduko Solved in "+((double)(stopTime - startTime)/1000000000.0)+"sec");
	bHelper.printValueBoard(sk[0].getResult());
	
	

}
}
