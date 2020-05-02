import java.util.*;

/*
 * 
 * Class to check if the soduko is valid or not
 * */
public class SodukoValidator {

	private int[][] board;
	private int size;
	private boolean[][] rows;
	private boolean[][] columns;
	private boolean[][] grids;
	private int smallGridSize;
	SodukoValidator(Cell[][] board, int n)
	{
		this.size =n;
		this.smallGridSize =(int) Math.sqrt(n);
		this.board = new int[size][size];
		for(int i=0;i<size;i++)
			for(int j=0;j<size;j++)
				if(board[i][j].isSet())
					this.board[i][j]= board[i][j].getIntValue();
				else
					this.board[i][j]=0;
		
		rows = new boolean[size][size];
		columns = new boolean[size][size];
		grids = new boolean[size][size];
			
	}
	
	// Method which checks the constraint for the soduko problem
	//If there are repeating number for a row , column or sub grid it will return false 
	public boolean validate()
	{
		for(int i=0;i<size;i++)
			for(int j=0;j<size;j++)
			{
				if(board[i][j]==0)
					continue;
				int value = board[i][j]-1;
				if(rows[i][value]==false)
					{
					rows[i][value]=true;
					//if(i==0&&value==1)
						//System.out.println("This is the col"+j);
						//System.out.println("Row "+i+" for "+value);
					}
					
				else
				{	if(i==0 && value==1)
						//System.out.println("This is DUP col"+j);
					System.out.println("Row "+i+" for "+value);
					return false;
					
				}
				if(columns[j][value]==false)
					columns[j][value]=true;
				
				else
					{
					System.out.println("Col "+j+" for "+value);
					return false;
					
					}
				int gridNum = i/smallGridSize*smallGridSize+j/smallGridSize;
				if(grids[gridNum][value]==false)
					grids[gridNum][value]=true;
				else
					{
					System.out.println("Grid "+gridNum+" for "+value);
					return false;
					
					}
				
			}
		return true;
	}
	
	
}
