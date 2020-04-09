import java.util.*;
public class SodukoValidator {

	private int[][] board;
	private int size;
	private boolean[][] rows;
	private boolean[][] columns;
	private boolean[][] grids;
	private int smallGridSize;
	SodukoValidator(int[][] board, int n)
	{
		this.size =n;
		this.smallGridSize =(int) Math.sqrt(n);
		this.board = new int[size][size];
		for(int i=0;i<size;i++)
			for(int j=0;j<size;j++)
				this.board[i][j]=board[i][j];
		rows = new boolean[size][size];
		columns = new boolean[size][size];
		grids = new boolean[size][size];
			
	}
	
	public boolean validate()
	{
		for(int i=0;i<size;i++)
			for(int j=0;j<size;j++)
			{
				if(board[i][j]==0)
					continue;
				int value = board[i][j]-1;
				if(rows[i][value]==false)
					rows[i][value]=true;
				else
					return false;
				if(columns[j][value]==false)
					columns[j][value]=true;
				else
					return false;
				int gridNum = i/smallGridSize*smallGridSize+j/smallGridSize;
				if(grids[gridNum][value]==false)
					grids[gridNum][value]=true;
				else
					return false;
				
			}
		return true;
	}
	
	
}
