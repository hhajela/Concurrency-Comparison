import java.util.*;
import java.lang.*;
public class SodukoSolver implements Runnable {
	
	private static Stack<Cell[][]> processingSolution=new Stack();
	
	private static Cell[][] result;
	private BoardHelper bHelper;
	private static int totalThread;
	private static int threadWaiting=0;
	
	 
	
	public SodukoSolver(int t)
	{
		
		bHelper = new BoardHelper();
		totalThread = t;
		result=null;
		
		
	}
	
	public Cell[][] getResult()
	{
		return result;
	}
	
	public boolean checkResult()
	{
		return (result!=null);
	}
	
	public synchronized void increaseWatingThread()
	{
		System.out.println(Thread.currentThread().getName()+" is waiting");
		threadWaiting++;
	}
	
	public synchronized void reduceWaitingThread()
	{
		threadWaiting--;
		System.out.println(Thread.currentThread().getName()+" has assignment");
	}
	
	public boolean isSolved(Cell[][] board)
	{
		int size = board.length;
		for(int i=0;i<size;i++)
			for(int j=0;j<size;j++)
				if(!board[i][j].isSet())
					return false;
		return true;
	}
	
	public boolean checkPotentialNotEmpty(Cell[][] board)
	{
		int size = board.length;
		for(int i=0;i<size;i++)
			for(int j=0;j<size;j++)
				if(board[i][j].getPotentialValuesSize()==0)
					return false;
		return true;
			
		
	}
	
		
	public boolean isValid(Cell[][] board)
	{
		int size = board.length;
		for(int i=0;i<size;i++)
			for(int j=0;j<size;j++)
				if(!board[i][j].isSet()&&board[i][j].getPotentialValuesSize()==1)
					{
					reduceWithIndex(board[i][j],board[i][j].getFirstPotentialValue(),board);
					}
		
		SodukoValidator sv = new SodukoValidator(board,size);
		
		return (checkPotentialNotEmpty(board)&&sv.validate());
	}
	
	public synchronized void addToStack(Cell[][] board)
	{
		processingSolution.push(board);
	}
	
	public synchronized Cell[][] RemoveFromStack()
	{
		while(processingSolution.empty())
			{
			System.out.println(Thread.currentThread().getName()+" is waiting inside RemoveFromStack");
			continue;
			}
		return this.processingSolution.pop();
	}
	
	@Override
	public void run() {
		
		
		try
		{
		while(!processingSolution.empty()||threadWaiting<totalThread)
		{	if(processingSolution.empty())
			{
				
				increaseWatingThread();
				
				synchronized(this)
				{
				while(processingSolution.empty())
				{
					if(threadWaiting<totalThread)
						continue;
					else
						return;
				}
				
				reduceWaitingThread();
				}
			}
			
			Cell[][] board = RemoveFromStack();
		
		
		
			if(isSolved(board))
				{
				result = board;
				}
			
			if(checkResult())
			{	
				return;
			}
			Cell selectedCell = minSelectCell(board);
		//System.out.print(Thread.currentThread().getName()+" ");
		//System.out.print("Minimum Selected ");	
		//selectedCell.print();
		for(int i=0;i<selectedCell.getPotentialValuesSize();i++)
		{	//System.out.println("Trying value "+selectedCell.getPotentialValue(i)+" for "+selectedCell.getRow()+" "+selectedCell.getCol());
		
			//next_grid = localBoard;
			Cell[][] next_grid = bHelper.getCopy(board);
			next_grid = reduceWithIndex(selectedCell,selectedCell.getPotentialValue(i),next_grid);
			//printBoard(next_grid);
			//printValueBoard(next_grid);
			if(isValid(next_grid))
			{ 	
				addToStack(next_grid);
				
			}
		}
	}
		}
		
	catch(Exception e)
		{
		System.out.println("In the exception block" +Thread.currentThread().getName());
		e.printStackTrace();
		}
	}
		
	
	
	
	public Cell[][] reduceWithIndex(Cell selectedcell, int value, Cell[][] board)
	{
		int i = selectedcell.getRow();
		int j = selectedcell.getCol();
		
		board[i][j].setValue();
		board[i][j].setIntValue(value);
		int size = board.length;
		int smallGridSize = (int) Math.sqrt(size);
		
		
		// This will remove the values for the whole column and rows
		for(int k=0;k<size;k++)
		{
			if(!board[i][k].isSet())
				{
				board[i][k].remove(value);
				//System.out.println("Removing "+value+"from+ "+i+" "+k+" Rowwise");
				}
			if(!board[k][j].isSet())
				{board[k][j].remove(value);
				//System.out.println("Removing "+value+"from+ "+k+" "+j+" Colwise");
				}
				
		}
		int gridrow = (i/smallGridSize)*smallGridSize;
		int gridcol =(j/smallGridSize)*smallGridSize;
		//This will remove the values for the respective grid
		for(int k=gridrow;k<gridrow+smallGridSize;k++)
			for(int l=gridcol;l<gridcol+smallGridSize;l++)
				{
					if(!board[k][l].isSet())
						{
						board[k][l].remove(value);
						//System.out.println("Removing "+value+"from+ "+k+" "+l+" Gridwise");
						}
				}
				
		return board;
		
	}
	
		
	public Cell minSelectCell(Cell[][] board)
	{
		int size = board.length;
		
		int minPossibility = size+1;
		Cell selectedCell=new Cell(0,0);
		for(int i=0;i<size;i++)
			for(int j=0;j<size;j++)
			{
				if(!board[i][j].isSet()&&board[i][j].getPotentialValuesSize()<minPossibility)
				{
					minPossibility = board[i][j].getPotentialValuesSize();
					selectedCell = board[i][j];
							
				}
			}
		return selectedCell;
	}



	



	
}
