import java.util.*;
import java.lang.*;
public class SodukoSolver {
	
	private int[][] board;
	private int size;
	private int smallGridSize;
	
	SodukoSolver(int[][] board,int n)
	{
		this.size =n;
		this.smallGridSize =(int) Math.sqrt(n);
		
		this.board = new int[size][size];
		for(int i=0;i<size;i++)
			for(int j=0;j<size;j++)
				this.board[i][j]=board[i][j];
	}
	
	public void beforeSolve()
	{
		Cell[][] cell = initializeNewCell(this.board);
		Cell[][] updated_cell = reduce(cell,this.board);
		
		
		minSelectCell(updated_cell).print();
		
	}
	
	
	
	public Cell[][] initializeNewCell(int[][] grid)
	{
		Cell[][] cell = new Cell[size][size];
		
		for(int i=0;i<size;i++)
			for(int j=0;j<size;j++)
			{
				cell[i][j]=new Cell(i,j);
				if(grid[i][j]==0)
				{
					for(int k=1;k<size;k++)
						cell[i][j].add(k);
					
				}
				else
				{
					cell[i][j].add(grid[i][j]);
					cell[i][j].setValue();
					
				}
					
			}
		return cell;
	}
	
	
	public Cell[][] reduce(Cell[][] cell,int[][] grid)
	{
		for(int i=0;i<size;i++)
			for(int j=0;j<size;j++)
			{
				if(grid[i][j]!=0)
				{
					// This will remove the values for the whole column and rows
					for(int k=0;k<size;k++)
					{
						if(!cell[i][k].isSet())
							cell[i][k].remove(grid[i][j]);
						if(!cell[k][j].isSet())
							cell[k][j].remove(grid[i][j]);
					}
					int gridrow = (i/smallGridSize)*smallGridSize;
					int gridcol =(j/smallGridSize)*smallGridSize;
					//This will remove the values for the respective grid
					for(int k=gridrow;k<gridrow+smallGridSize;k++)
						for(int l=gridcol;l<gridcol+smallGridSize;l++)
						{
							if(!cell[k][l].isSet())
								{
								cell[k][l].remove(grid[i][j]);
								}
						}
				}
				
			}
		return cell;
		
	}
	
	public Cell[][] reduceWithIndex(Cell[][] cell,int[][] grid,Cell selectedcell)
	{
		int i = selectedcell.getRow();
		int j = selectedcell.getCol();
		
		cell[i][j].setValue();
		
		int value  =(int) selectedcell.getPotentialValues();
		
		
		// This will remove the values for the whole column and rows
		for(int k=0;k<size;k++)
		{
			if(!cell[i][k].isSet())
				cell[i][k].remove(value);
			if(!cell[k][j].isSet())
				cell[k][j].remove(value);
		}
		int gridrow = (i/smallGridSize)*smallGridSize;
		int gridcol =(j/smallGridSize)*smallGridSize;
		//This will remove the values for the respective grid
		for(int k=gridrow;k<gridrow+smallGridSize;k++)
			for(int l=gridcol;l<gridcol+smallGridSize;l++)
				{
					if(!cell[k][l].isSet())
						{
							cell[k][l].remove(value);
						}
				}
				
		return cell;
		
	}
	
	public Cell minSelectCell(Cell[][] cell)
	{
		int minPossibility = size+1;
		Cell selectedCell = new Cell(0,0);
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
