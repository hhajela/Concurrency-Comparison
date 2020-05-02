import java.util.*;
import java.lang.*;

/*
 * 
 * This class solves the soduko problem using CSP algorithm
 * Implements runnable for running it as multithreading solution
 * */
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
	
	//Function which returns final result
	public Cell[][] getResult()
	{
		return result;
	}
	
	//Check if some thread found the solutiotn
	public boolean checkResult()
	{
		return (result!=null);
	}
	
	//Increase the count of number of waiting thread
	public synchronized void increaseWatingThread()
	{
		System.out.println(Thread.currentThread().getName()+" is waiting");
		threadWaiting++;
	}
	
	//Decrease the count of number of waiting thread
	public synchronized void reduceWaitingThread()
	{
		threadWaiting--;
		System.out.println(Thread.currentThread().getName()+" has assignment");
	}
	
	
	//Check if the grid is already solved
	public boolean isSolved(Cell[][] board)
	{
		int size = board.length;
		for(int i=0;i<size;i++)
			for(int j=0;j<size;j++)
				if(!board[i][j].isSet())
					return false;
		return true;
	}
	
	//Check after the updating the grid, if there is a cell with no possible solution
	public boolean checkPotentialNotEmpty(Cell[][] board)
	{
		int size = board.length;
		for(int i=0;i<size;i++)
			for(int j=0;j<size;j++)
				if(board[i][j].getPotentialValuesSize()==0)
					return false;
		return true;
			
		
	}
	
	//Check if the grid is still valid based on soduko solution
	public boolean isValid(Cell[][] board)
	{
		int size = board.length;
		
		//Loop to set values for the cell which have only one possible solution left
		for(int i=0;i<size;i++)
			for(int j=0;j<size;j++)
				if(!board[i][j].isSet()&&board[i][j].getPotentialValuesSize()==1)
					{
					reduceWithIndex(board[i][j],board[i][j].getPotentialValue(0),board);
					}
		
		SodukoValidator sv = new SodukoValidator(board,size);
		
		return (checkPotentialNotEmpty(board)&&sv.validate());
	}
	
	//Function which add new grid to stack
	public synchronized void addToStack(Cell[][] board)
	{
		processingSolution.push(board);
	}
	
	//Function which remove grid from stack
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
			//Will continue either if the stack has some element or thread still some thread working on the problem
		while(!processingSolution.empty()||threadWaiting<totalThread)
		{	
			//If stack is empty increase the waiting thread
			if(processingSolution.empty())
			{
				
				increaseWatingThread();
				
				
				synchronized(this)
				{
					// This is to make thread wait for new grid to be pushed to the stack
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
			
			//Assigning work to thread
			Cell[][] board = RemoveFromStack();
		
		
			//
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
		
	
	
	//This function set the value of the minimum selected cell and update the other
	//by removing the set value from their potential list
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
	
	//Function which finds the cell which has minimum possible solution left	
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
