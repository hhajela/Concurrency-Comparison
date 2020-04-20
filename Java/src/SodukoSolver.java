import java.util.*;
import java.lang.*;
public class SodukoSolver {
	
	private Cell[][] board;
	private int size;
	private int smallGridSize;
	
	SodukoSolver(int[][] board,int n)
	{
		this.size =n;
		this.smallGridSize =(int) Math.sqrt(n);
		
		this.board =new Cell[size][size];
		
		for(int i=0;i<size;i++)
			for(int j=0;j<size;j++)
			{
				this.board[i][j]=new Cell(i,j);
				if(board[i][j]==0)
				{
					for(int k=1;k<=size;k++)
						this.board[i][j].add(k);
					
				}
				else
				{
					this.board[i][j].add(board[i][j]);
					this.board[i][j].setValue();
					this.board[i][j].setIntValue(board[i][j]);
					
				}
					
			}
		reduce(board);
		
		
	}
	
	public void reduce(int[][] grid)
	{
		for(int i=0;i<size;i++)
			for(int j=0;j<size;j++)
			{
				if(grid[i][j]!=0)
				{
					// This will remove the values for the whole column and rows
					for(int k=0;k<size;k++)
					{
						if(!this.board[i][k].isSet())
							
							{this.board[i][k].remove(grid[i][j]);
							//System.out.println("Removing "+grid[i][j]+" from "+i+" "+k+" Rowise");
							}
						if(!this.board[k][j].isSet())
							{
							this.board[k][j].remove(grid[i][j]);
							//System.out.println("Removing "+grid[i][j]+" from+ "+k+" "+j+" Colwise");
							}
					}
					int gridrow = (i/smallGridSize)*smallGridSize;
					int gridcol =(j/smallGridSize)*smallGridSize;
					//This will remove the values for the respective grid
					for(int k=gridrow;k<gridrow+smallGridSize;k++)
						for(int l=gridcol;l<gridcol+smallGridSize;l++)
						{
							if(!this.board[k][l].isSet())
								{
								this.board[k][l].remove(grid[i][j]);
								//System.out.println("Removing "+grid[i][j]+"from+ "+k+" "+l+" Gridwise");
								}
						}
				}
				
			}
		
	}
	
	
	public boolean isSolved(Cell[][] cell)
	{
		for(int i=0;i<size;i++)
			for(int j=0;j<size;j++)
				if(!cell[i][j].isSet())
					return false;
		return true;
	}
	
		
	public boolean isValid(Cell[][] intermediateCell)
	{
		for(int i=0;i<size;i++)
			for(int j=0;j<size;j++)
				if(!intermediateCell[i][j].isSet()&&intermediateCell[i][j].getPotentialValuesSize()==1)
					{
					//intermediateCell[i][j].setValue();
					//intermediateCell[i][j].setIntValue(intermediateCell[i][j].getFirstPotentialValue());
					//System.out.println("Setting "+intermediateCell[i][j].getFirstPotentialValue()+" for "+i+" "+j);
					reduceWithIndex(intermediateCell[i][j],intermediateCell[i][j].getFirstPotentialValue(),intermediateCell);
					}
		
		SodukoValidator sv = new SodukoValidator(intermediateCell,size);
		
		return sv.validate();
	}
	
	
	public Cell[][] getboard()
	{
		return this.board;
	}
	
	public Cell[][] solve(Cell[][] localBoard)
	{
		
		//printBoard(localBoard);
		if(isSolved(localBoard))
			return localBoard;
		
		Cell selectedCell = minSelectCell(localBoard);
		System.out.print("Minimum Selected ");
		selectedCell.print();
		for(int i=0;i<selectedCell.getPotentialValuesSize();i++)
		{	System.out.println("Trying value "+selectedCell.getPotentialValue(i)+" for "+selectedCell.getRow()+" "+selectedCell.getCol());
			
			//next_grid = localBoard;
			Cell[][] next_grid = getCopy(localBoard);
			next_grid = reduceWithIndex(selectedCell,selectedCell.getPotentialValue(i),next_grid);
			//printBoard(next_grid);
			printValueBoard(next_grid);
			if(isValid(next_grid))
			{ Cell[][] sol =solve(next_grid);
				 if(isSolved(sol))
				 	return sol;
				 else
					 continue;
			}
		}
		return localBoard;
		
		
			
		
	}
	
	@SuppressWarnings("unchecked")
	public Cell[][] getCopy(Cell[][] board)
	{
		Cell[][] local = new Cell[size][size];
		
		for(int i=0;i<size;i++)
			for(int j=0;j<size;j++)
				{
				local[i][j]= new Cell(board[i][j].getRow(),board[i][j].getCol());
				local[i][j].setPV((ArrayList<Integer>) board[i][j].getPV().clone());
				if(board[i][j].isSet())
					local[i][j].setValue();
				local[i][j].setIntValue(board[i][j].getIntValue());
				}
		
		return local;
		
	}
	
	public void printBoard(Cell[][] board)
	{
		for(int i=0;i<size;i++)
			for(int j=0;j<size;j++)
				board[i][j].print();
	}
	
	public Cell[][] reduceWithIndex(Cell selectedcell, int value, Cell[][] intermediateCell)
	{
		int i = selectedcell.getRow();
		int j = selectedcell.getCol();
		
		intermediateCell[i][j].setValue();
		intermediateCell[i][j].setIntValue(value);
		
		
		
		// This will remove the values for the whole column and rows
		for(int k=0;k<size;k++)
		{
			if(!intermediateCell[i][k].isSet())
				{
				intermediateCell[i][k].remove(value);
				//System.out.println("Removing "+value+"from+ "+i+" "+k+" Rowwise");
				}
			if(!intermediateCell[k][j].isSet())
				{intermediateCell[k][j].remove(value);
				//System.out.println("Removing "+value+"from+ "+k+" "+j+" Colwise");
				}
				
		}
		int gridrow = (i/smallGridSize)*smallGridSize;
		int gridcol =(j/smallGridSize)*smallGridSize;
		//This will remove the values for the respective grid
		for(int k=gridrow;k<gridrow+smallGridSize;k++)
			for(int l=gridcol;l<gridcol+smallGridSize;l++)
				{
					if(!intermediateCell[k][l].isSet())
						{
						intermediateCell[k][l].remove(value);
						//System.out.println("Removing "+value+"from+ "+k+" "+l+" Gridwise");
						}
				}
				
		return intermediateCell;
		
	}
	
	public void printValueBoard(Cell[][] board)
	{
		for(int i=0;i<size;i++)
		{for(int j=0;j<size;j++)
			{
			System.out.print(board[i][j].getIntValue()+" ");
			}
			System.out.print("\n");
		}
	}
	
	public Cell minSelectCell(Cell[][] cell)
	{
		int minPossibility = size+1;
		Cell selectedCell=new Cell(0,0);
		for(int i=0;i<size;i++)
			for(int j=0;j<size;j++)
			{
				if(!cell[i][j].isSet()&&cell[i][j].getPotentialValuesSize()<minPossibility)
				{
					minPossibility = cell[i][j].getPotentialValuesSize();
					selectedCell = cell[i][j];
							
				}
			}
		return selectedCell;
	}
}
