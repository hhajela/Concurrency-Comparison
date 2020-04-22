import java.util.ArrayList;

public class Board {
	
	private Cell[][] board;
	private int size;
	private int smallGridSize;
	
	Board(int[][] board,int n)
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
		System.out.println("This is new");
		
		
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
	
	
	public Cell[][] getBoard()
	{
		return this.board;
	}
	
	public int getSize()
	{
		return size;
	}
	
	public int getSmallGridSize()
	{
		return this.smallGridSize;
	}
	
	
}
